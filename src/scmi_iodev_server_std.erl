%%% The MIT License
%%%
%%% Copyright (C) 2013-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme interpreter i/o device stdio and stderr server
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_iodev_server_std).
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
-export_type([mode/0, mode_error/0, opt/0]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type server()     :: pid().
-type device()     :: standard_io | standard_error.

-type mode()       :: read | write.
-type mode_error() :: write.
-type opt()        :: {'read', boolean()}
                    | {'write', boolean()}
                    | {'binary', false}
                    | {'test', boolean()}.

-record(state, {
          read=false :: boolean(),
          write=false :: boolean(),
          test :: pid(),
          device :: device()
         }).

%%%----------------------------------------------------------------------
%%% API - client
%%%----------------------------------------------------------------------

-spec start(standard_io, [mode()]) -> {ok, pid()} | ignore | {error, Reason::term()};
           (standard_error, [mode_error()]) -> {ok, pid()} | ignore | {error, Reason::term()}.
start(Term, Modes) ->
    gen_server:start(?MODULE, {Term, Modes}, []).

-spec getopts(server()) -> [opt()].
getopts(Server) ->
    case call(Server, getopts) of
        #state{read=Read, write=Write, test=Test} ->
            [{read,Read}, {write,Write}, {binary,false}, {test,Test/=undefined}];
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
        #state{} when Mode == binary ->
            false;
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

-spec read(server(), non_neg_integer()) -> string() | eof | {error, Reason::term()}.
read(Server, K) ->
    call(Server, {read, K}).

-spec read_all(server()) -> string() | eof | {error, Reason::term()}.
read_all(Server) ->
    call(Server, read_all).

-spec read_line(server()) -> string() | eof | {error, Reason::term()}.
read_line(Server) ->
    call(Server, read_line).

-spec peek(server(), non_neg_integer()) -> string() | eof | {error, Reason::term()}.
peek(Server, K) ->
    call(Server, {peek, K}).

-spec write(server(), string()) -> ok | {error, Reason::term()}.
write(Server, Data) ->
    call(Server, {write, Data}).

-spec flush(server()) -> ok | {error, Reason::term()}.
flush(Server) ->
    call(Server, flush).

%%%----------------------------------------------------------------------
%%% API - server
%%%----------------------------------------------------------------------

-spec init({iodata() | standard_io | standard_error, [mode()]}) -> {ok, #state{}}.
init({Term, Modes}) ->
    put('scmi_iodev_server', ?MODULE),

    Read = proplists:get_bool(read, Modes),
    Write = proplists:get_bool(write, Modes),
    Test = proplists:get_bool(test, Modes),
    TestServer = if Test -> spawn_link(scmi_iodev_server_std_test, test_server, [self()]); true -> undefined end,

    case Term of
        standard_io when Read orelse Write ->
            ok = io:setopts(Term, [{binary, false}] ++ [{encoding, unicode}]),
            {ok, #state{read=Read, write=Write, test=TestServer, device=Term}};
        standard_io ->
            {stop, badarg};
        standard_error when not Read ->
            {ok, #state{read=Read, write=Write, test=TestServer, device=Term}};
        standard_error ->
            {stop, badarg}
    end.

handle_call(Msg, From, #state{test=undefined}=State) ->
    handle_call1(Msg, From, State);
handle_call(Msg, From, #state{test=TestServer}=State) ->
    OldGL = group_leader(),
    group_leader(TestServer, self()),
    try
        handle_call1(Msg, From, State)
    after
        group_leader(OldGL, self())
    end.

handle_call1(getopts, _From, State) ->
    {reply, State, State};
handle_call1(close, _From, State) ->
    {Reply, #state{device=Device}=NewState} = do_close(State),
    if Device == undefined ->
            {stop, normal, Reply, NewState};
       true ->
            {reply, Reply, NewState}
    end;
handle_call1({close, Mode}, _From, State) ->
    {Reply, #state{device=Device}=NewState} = do_close(Mode, State),
    if Device == undefined ->
            {stop, normal, Reply, NewState};
       true ->
            {reply, Reply, NewState}
    end;
handle_call1(is_ready, _From, State) ->
    {Reply, NewState} = do_is_ready(State),
    {reply, Reply, NewState};
handle_call1({read, K}, _From, State) ->
    {Reply, NewState} = do_read(K, State),
    {reply, Reply, NewState};
handle_call1(read_all, _From, State) ->
    {Reply, NewState} = do_read_all(State),
    {reply, Reply, NewState};
handle_call1(read_line, _From, State) ->
    {Reply, NewState} = do_read_line(State),
    {reply, Reply, NewState};
handle_call1({peek, K}, _From, State) ->
    {Reply, NewState} = do_peek(K, State),
    {reply, Reply, NewState};
handle_call1({write, Data}, _From, State) ->
    {Reply, NewState} = do_write(Data, State),
    {reply, Reply, NewState};
handle_call1(flush, _From, State) ->
    {Reply, NewState} = do_flush(State),
    {reply, Reply, NewState};
handle_call1(_Request, _From, State) ->
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
do_close(State) ->
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
    {{error, {roadmap,'v0.9.0'}}, State};
do_is_ready(State) ->
    {{error, ebadf}, State}.

%% read/2
do_read(K, #state{read=true, device=Device}=State) ->
    Reply = io:get_chars(Device, '', K),
    {Reply, State};
do_read(_K, State) ->
    {{error, ebadf}, State}.

%% read_all/1
do_read_all(#state{read=true}=State) ->
    {{error, enotsup}, State}; % unsupported
do_read_all(State) ->
    {{error, ebadf}, State}.

%% read_line/1
do_read_line(#state{read=true, device=Device}=State) ->
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
do_peek(_K, #state{read=true}=State) ->
    {{error, {roadmap,'v0.9.0'}}, State};
do_peek(_K, State) ->
    {{error, ebadf}, State}.

%% write/2
do_write(Data, #state{write=true}=State) when is_binary(Data) ->
    {{error, badarg}, State};
do_write(Data, #state{write=true, device=Device}=State) ->
    Reply = io:put_chars(Device, Data),
    {Reply, State};
do_write(_Data, State) ->
    {{error, ebadf}, State}.

%% flush/1
do_flush(#state{write=true}=State) ->
    {ok, State}; % no-op
do_flush(State) ->
    {{error, ebadf}, State}.
