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

%%% @doc Scheme interpreter i/o device stdio and stderr test server
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_iodev_server_std_test).

%% External exports
-export[test_server/1].

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

test_server(Parent) ->
    test_server(Parent, []).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

test_server(Parent, L) ->
    receive
        die ->
            ok;
        {io_request,From,To,{put_chars,_Encoding,Data}}=Msg ->
            Reply = {io_reply, To, ok},
            txn(Parent, L, Msg, Reply, From),
            test_server(Parent, L ++ to_list(Data));
        {io_request,From,To,{get_chars,_Encoding,'',_N}}=Msg when L==[] ->
            Reply = {io_reply, To, eof},
            txn(Parent, L, Msg, Reply, From),
            test_server(Parent, L);
        {io_request,From,To,{get_chars,_Encoding,'',N}}=Msg ->
            Data = lists:sublist(L, 1, N),
            Reply = {io_reply, To, Data},
            txn(Parent, L, Msg, Reply, From),
            test_server(Parent, lists:nthtail(erlang:min(N,length(L)), L));
        {io_request,From,To,{get_line,_Encoding,''}}=Msg when L==[] ->
            Reply = {io_reply, To, eof},
            txn(Parent, L, Msg, Reply, From),
            test_server(Parent, L);
        {io_request,From,To,{get_line,_Encoding,''}}=Msg ->
            {N, Data} = test_read_line(L),
            Reply = {io_reply, To, Data},
            txn(Parent, L, Msg, Reply, From),
            test_server(Parent, lists:nthtail(erlang:min(N,length(L)), L));
        Msg ->
            io:format("Unhandled ~p~n", [Msg]),
            erlang:error(unhandled, [Msg])
    end.

txn(_Parent, _L, _Msg, Reply, From) ->
    From ! Reply,
    ok.

to_list(L) when is_list(L) ->
    L;
to_list(B) ->
    unicode:characters_to_list(B, unicode).

test_read_line(L) ->
    test_read_line(L, 0, []).

test_read_line([], N, Acc) ->
    {N, lists:reverse(Acc)};
test_read_line([$\r|[$\r|_]=T], N, Acc) ->
    test_read_line(T, N+1, Acc);
test_read_line([$\r,$\n|_], N, Acc) ->
    {N+2, lists:reverse(Acc)++ "\n"};
test_read_line([$\r|_], N, Acc) ->
    {N+1, lists:reverse(Acc) ++ "\n"};
test_read_line([$\n|_], N, Acc) ->
    {N+1, lists:reverse(Acc) ++ "\n"};
test_read_line([H|T], N, Acc) ->
    test_read_line(T, N+1, [H|Acc]).
