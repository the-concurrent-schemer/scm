%%% The MIT License
%%%
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Scheme interpreter i/o device file server
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_iodev_server_file).
-behaviour(gen_server).

%% External exports
-export([%% client
         start/2
         , getopts/1, getopts/2
         , close/1, close/2
         , is_ready/1
         , read/2
         , read_all/1
         , read_line/1
         , peek/2
         , write/2
         , flush/1
         %% server
         , init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3
        ]).

%% External types
-export_type([filename/0, mode/0, opt/0]).

-include_lib("kernel/include/file.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type server()   :: pid().
-type device()   :: file:io_device().
-type filename() :: file:filename_all().

-type mode()     :: read | write | append | exclusive | binary.
-type opt()      :: {'read', boolean()}
                  | {'write', boolean()}
                  | {'binary', boolean()}.

-record(state, {
          read=false :: boolean(),
          write=false :: boolean(),
          binary=false :: boolean(),
          device :: device(),
          filename :: filename()
         }).

%%%----------------------------------------------------------------------
%%% API - client
%%%----------------------------------------------------------------------

-spec start(iodata(), [mode()]) -> {ok, pid()} | ignore | {error, Reason::term()}.
start(Term, Modes) ->
    gen_server:start(?MODULE, {Term, Modes}, []).

-spec getopts(server()) -> [opt()].
getopts(Server) ->
    case call(Server, getopts) of
        #state{read=Read, write=Write, binary=Binary} ->
            [{read,Read}, {write,Write}, {binary,Binary}];
        Err ->
            Err
    end.

-spec getopts(server(), read | write | binary) -> boolean().
getopts(Server, Mode) ->
    case call(Server, getopts) of
        #state{read=Read} when Mode == read ->
            Read;
        #state{write=Write} when Mode == write ->
            Write;
        #state{binary=Binary} when Mode == binary ->
            Binary;
        #state{} ->
            erlang:error(badarg, [Server, Mode]);
        Err ->
            Err
    end.

-spec close(server()) -> boolean().
close(Server) ->
    call(Server, close).

-spec close(server(), read | write) -> boolean().
close(Server, Mode) ->
    call(Server, {close, Mode}).

-spec is_ready(server()) -> boolean().
is_ready(Server) ->
    call(Server, is_ready).

-spec read(server(), non_neg_integer()) -> string() | binary() | eof | {error, Reason::term()}.
read(Server, K) ->
    call(Server, {read, K}).

-spec read_all(server()) -> string() | binary() | eof | {error, Reason::term()}.
read_all(Server) ->
    call(Server, read_all).

-spec read_line(server()) -> string() | binary() | eof | {error, Reason::term()}.
read_line(Server) ->
    call(Server, read_line).

-spec peek(server(), non_neg_integer()) -> string() | binary() | eof | {error, Reason::term()}.
peek(Server, K) ->
    call(Server, {peek, K}).

-spec write(server(), string() | binary()) -> ok | {error, Reason::term()}.
write(Server, Data) ->
    call(Server, {write, Data}).

-spec flush(server()) -> ok | {error, Reason::term()}.
flush(Server) ->
    call(Server, flush).

%%%----------------------------------------------------------------------
%%% API - server
%%%----------------------------------------------------------------------

-spec init({iodata(), [mode()]}) -> {ok, #state{}}.
init({Term, Modes}) ->
    put('scmi_iodev_server', ?MODULE),

    Read = proplists:get_bool(read, Modes),
    Append = proplists:get_bool(append, Modes),
    Write = proplists:get_bool(write, Modes) orelse Append,
    Exclusive = proplists:get_bool(exclusive, Modes),
    Binary = proplists:get_bool(binary, Modes),

    if Read orelse Write ->
            OpenModes = [read || Read] ++ [write || Write] ++ [append || Append] ++ [exclusive || Exclusive]
                ++ [binary || Binary]
                ++ [{encoding, unicode} || not Binary],

            case file:open(Term, OpenModes) of
                {ok, Device} ->
                    {ok, #state{read=Read, write=Write, binary=Binary, device=Device, filename=Term}};
                {error, Reason} ->
                    {stop, Reason}
            end;
       true ->
            {stop, badarg}
    end.

handle_call(getopts, _From, State) ->
    {reply, State, State};
handle_call(close, _From, State) ->
    {Reply, #state{device=Device}=NewState} = do_close(State),
    if Device == undefined ->
            {stop, normal, Reply, NewState};
       true ->
            {reply, Reply, NewState}
    end;
handle_call({close, Mode}, _From, State) ->
    {Reply, #state{device=Device}=NewState} = do_close(Mode, State),
    if Device == undefined ->
            {stop, normal, Reply, NewState};
       true ->
            {reply, Reply, NewState}
    end;
handle_call(is_ready, _From, State) ->
    {Reply, NewState} = do_is_ready(State),
    {reply, Reply, NewState};
handle_call({read, K}, _From, State) ->
    {Reply, NewState} = do_read(K, State),
    {reply, Reply, NewState};
handle_call(read_all, _From, State) ->
    {Reply, NewState} = do_read_all(State),
    {reply, Reply, NewState};
handle_call(read_line, _From, State) ->
    {Reply, NewState} = do_read_line(State),
    {reply, Reply, NewState};
handle_call({peek, K}, _From, State) ->
    {Reply, NewState} = do_peek(K, State),
    {reply, Reply, NewState};
handle_call({write, Data}, _From, State) ->
    {Reply, NewState} = do_write(Data, State),
    {reply, Reply, NewState};
handle_call(flush, _From, State) ->
    {Reply, NewState} = do_flush(State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(close, State) ->
    {_, NewState} = do_close(State),
    {stop, normal, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    _ = do_close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

call(Pid, Msg) ->
    %% @TODO choose (if any) how to handle errors
    gen_server:call(Pid, Msg).

%% close/1
do_close(#state{device=undefined}=State) ->
    {false, State};
do_close(#state{device=Device}=State) ->
    ok = file:close(Device),
    {true, State#state{device=undefined, read=false, write=false}}.

%% close/2
do_close(read, #state{read=true, write=true}=State) ->
    {true, State#state{read=false}};
do_close(read, #state{read=true}=State) ->
    do_close(State#state{read=false});
do_close(write, #state{write=true, read=true}=State) ->
    {true, State#state{write=false}};
do_close(write, #state{write=true}=State) ->
    do_close(State#state{write=false});
do_close(_Mode, State) ->
    {false, State}.

%% is_ready/1
do_is_ready(#state{read=true}=State) ->
    {true, State};
do_is_ready(State) ->
    {{error, ebadf}, State}.

%% read/2
do_read(K, #state{read=true, device=Device}=State) ->
    Reply = io:get_chars(Device, '', K),
    {Reply, State};
do_read(_K, State) ->
    {{error, ebadf}, State}.

%% read_all/1
do_read_all(#state{read=true, device=Device, filename=Filename}=State) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{size=0}} ->
            case file:position(Device, bof) of
                {ok, 0} ->
                    {eof, State};
                {error, _} = Err ->
                    {Err, State}
            end;
        {ok, #file_info{size=Size}} ->
            case file:position(Device, bof) of
                {ok, 0} ->
                    do_read(Size, State);
                {error, _} = Err ->
                    {Err, State}
            end;
        Err ->
            {Err, State}
    end;
do_read_all(State) ->
    {{error, ebadf}, State}.

%% read_line/1
do_read_line(#state{read=true, binary=false, device=Device}=State) ->
    Reply = case io:get_line(Device, '') of
                eof ->
                    eof;
                {error, _} = Err ->
                    Err;
                Line ->
                    Line -- "\n"
            end,
    {Reply, State};
do_read_line(State) ->
    {{error, ebadf}, State}.

%% peek/2
do_peek(K, #state{read=true, device=Device}=State) ->
    case file:position(Device, cur) of
        {ok, Cur} ->
            try
                Reply = io:get_chars(Device, '', K),
                {Reply, State}
            after
                {ok, Cur} = file:position(Device, Cur)
            end;
        Err ->
            {Err, State}
    end;
do_peek(_K, State) ->
    {{error, ebadf}, State}.

%% write/2
do_write(Data, #state{write=true, binary=true}=State) when is_list(Data) ->
    {{error, badarg}, State};
do_write(Data, #state{write=true, binary=false}=State) when is_binary(Data) ->
    {{error, badarg}, State};
do_write(Data, #state{write=true, device=Device}=State) ->
    Reply = io:put_chars(Device, Data),
    {Reply, State};
do_write(_Data, State) ->
    {{error, ebadf}, State}.

%% flush/1
do_flush(#state{write=true, device=Device}=State) ->
    Reply = file:datasync(Device),
    {Reply, State};
do_flush(State) ->
    {{error, ebadf}, State}.
