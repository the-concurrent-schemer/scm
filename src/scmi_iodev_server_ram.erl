%%% The MIT License
%%%
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme interpreter i/o device ram server
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_iodev_server_ram).
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
-export_type([mode/0, opt/0]).

-include_lib("kernel/include/file.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type server()   :: pid().

-type mode()     :: read | write | append | binary.
-type opt()      :: {'read', boolean()}
                  | {'write', boolean()}
                  | {'binary', boolean()}.

-record(state, {
          read=false :: boolean(),
          write=false :: boolean(),
          binary=false :: boolean(),
          pos :: non_neg_integer(),
          len :: non_neg_integer(),
          data :: iodata()
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
    Binary = proplists:get_bool(binary, Modes),

    case {Binary, Term} of
        {true, Term} when is_binary(Term) ->
            Data = Term;
        {true, Term} ->
            Data = unicode:characters_to_binary(Term, unicode, unicode);
        {false, Term}  ->
            Data = unicode:characters_to_list(Term, unicode)
    end,

    case Data of
        {error, _, _} ->
            {stop, badarg};
        {incomplete, _, _} ->
            {stop, badarg};
        _ ->
            case {Binary, Append} of
                {true, true} ->
                    Len = byte_size(Data),
                    Pos = Len;
                {true, false} ->
                    Len = byte_size(Data),
                    Pos = 0;
                {false, true} ->
                    Len = length(Data),
                    Pos = Len;
                {false, false} ->
                    Len = length(Data),
                    Pos = 0
            end,

            if Read orelse Write ->
                    {ok, #state{read=Read, write=Write, binary=Binary, pos=Pos, len=Len, data=Data}};
               true ->
                    {stop, badarg}
            end
    end.

handle_call(getopts, _From, State) ->
    {reply, State, State};
handle_call(close, _From, State) ->
    {Reply, #state{data=Data}=NewState} = do_close(State),
    if Data == undefined ->
            {stop, normal, Reply, NewState};
       true ->
            {reply, Reply, NewState}
    end;
handle_call({close, Mode}, _From, State) ->
    {Reply, #state{data=Data}=NewState} = do_close(Mode, State),
    if Data == undefined ->
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
do_close(#state{data=undefined}=State) ->
    {false, State};
do_close(State) ->
    {true, State#state{data=undefined, read=false, write=false}}.

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
do_read(_K, #state{read=true, pos=Pos, len=Pos}=State) ->
    {eof, State};
do_read(K, #state{read=true, binary=false, pos=Pos, len=_Len, data=Data}=State) ->
    Reply = lists:sublist(Data, Pos+1, K),
    EffK = min(K, length(Reply)),
    {Reply, State#state{pos=Pos + EffK}};
do_read(K, #state{read=true, binary=true, pos=Pos, len=Len, data=Data}=State) ->
    EffK = min(K, Len - Pos),
    Reply = binary:part(Data, Pos, EffK),
    {Reply, State#state{pos=Pos + EffK}};
do_read(_K, State) ->
    {{error, ebadf}, State}.

%% read_all/1
do_read_all(#state{read=true, data=Data}=State) when Data == []; Data == <<>> ->
    {eof, State#state{pos=0}};
do_read_all(#state{read=true, binary=false, data=Data}=State) ->
    Len = length(Data),
    {Data, State#state{pos=Len}};
do_read_all(#state{read=true, binary=true, data=Data}=State) ->
    Len = byte_size(Data),
    {Data, State#state{pos=Len}};
do_read_all(State) ->
    {{error, ebadf}, State}.

%% read_line/1
do_read_line(#state{read=true, binary=Binary, pos=Pos, len=Pos}=State) when not Binary ->
    {eof, State};
do_read_line(#state{read=true, binary=Binary, pos=Pos, data=Data}=State) when not Binary ->
    {Offset, Reply} = do_read_line_list(lists:nthtail(Pos, Data)),
    {Reply, State#state{pos=Pos + Offset}};
do_read_line(State) ->
    {{error, ebadf}, State}.

do_read_line_list(L) ->
    do_read_line_list(L, 0, []).

do_read_line_list([], N, Acc) ->
    {N, lists:reverse(Acc)};
do_read_line_list([$\r|[$\r|_]=T], N, Acc) ->
    do_read_line_list(T, N+1, Acc);
do_read_line_list([$\r,$\n|_], N, Acc) ->
    {N+2, lists:reverse(Acc)};
do_read_line_list([$\r|_], N, Acc) ->
    {N+1, lists:reverse(Acc)};
do_read_line_list([$\n|_], N, Acc) ->
    {N+1, lists:reverse(Acc)};
do_read_line_list([H|T], N, Acc) ->
    do_read_line_list(T, N+1, [H|Acc]).

%% peek/2
do_peek(_K, #state{read=true, pos=Pos, len=Pos}=State) ->
    {eof, State};
do_peek(K, #state{read=true, binary=false, pos=Pos, len=_Len, data=Data}=State) ->
    Reply = lists:sublist(Data, Pos+1, K),
    {Reply, State};
do_peek(K, #state{read=true, binary=true, pos=Pos, len=Len, data=Data}=State) ->
    EffK = min(K, Len - Pos),
    Reply = binary:part(Data, Pos, EffK),
    {Reply, State};
do_peek(_K, State) ->
    {{error, ebadf}, State}.

%% write/2
do_write(D, #state{write=true, binary=true}=State) when is_list(D) ->
    {{error, badarg}, State};
do_write(D, #state{write=true, binary=false}=State) when is_binary(D) ->
    {{error, badarg}, State};
do_write([], #state{write=true, binary=false}=State) ->
    {ok, State};
do_write(<<>>, #state{write=true, binary=true}=State) ->
    {ok, State};
do_write(D0, #state{write=true, binary=false, pos=Pos, len=Len, data=Data}=State) ->
    D = unicode:characters_to_list(D0, unicode),
    L = length(D),
    {NewLen, NewData} = do_write_list(Pos, Len, Data, L, D),
    {ok, State#state{pos=Pos+L, len=NewLen, data=NewData}};
do_write(D, #state{write=true, binary=true, pos=Pos, len=Len, data=Data}=State) ->
    L = byte_size(D),
    {NewLen, NewData} = do_write_binary(Pos, Len, Data, L, D),
    {ok, State#state{pos=Pos+L, len=NewLen, data=NewData}};
do_write(_D, State) ->
    {{error, ebadf}, State}.

do_write_list(Pos, Pos, Data, L, D) ->
    {Pos+L, Data ++ D};
do_write_list(Pos, Len, Data, L, D) when Pos + L >= Len ->
    Data1 = lists:sublist(Data, Pos),
    {Pos+L, Data1 ++ D};
do_write_list(Pos, Len, Data, L, D) ->
    {Data1, Data3} = lists:split(Pos, Data),
    {Len, lists:append([Data1, D, lists:nthtail(L, Data3)])}.

do_write_binary(Pos, Pos, Data, L, D) ->
    {Pos+L, <<Data/binary, D/binary>>};
do_write_binary(Pos, Len, Data, L, D) ->
    case Data of
        <<Data1:Pos/binary, _:L/binary, Data3/binary>> ->
            {Len, <<Data1/binary, D/binary, Data3/binary>>};
        <<Data1:Pos/binary, _/binary>> ->
            {Pos + L, <<Data1/binary, D/binary>>}
    end.

%% flush/1
do_flush(#state{write=true}=State) ->
    {ok, State}; % no-op
do_flush(State) ->
    {{error, ebadf}, State}.
