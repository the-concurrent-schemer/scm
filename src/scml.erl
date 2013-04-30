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

%%% @doc Scheme library
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml).
-compile({no_auto_import,[binary_part/2, binary_part/3]}).

%% External exports
-export([libraries/0
         , list_part/2, list_part/3
         , tuple_part/2, tuple_part/3
         , binary_part/2, binary_part/3
         , to_unicode/1, to_utf8/1
         , unicode_to_utf8/1, unicode_to_utf8/2, unicode_to_utf8/3
         , utf8_to_unicode/1, utf8_to_unicode/2, utf8_to_unicode/3
        ]).

-include("scml.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type unichar() :: scmd_types_impl:unichar().
-type utf8() :: scmd_types_impl:utf8().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc This function is a temporary place holder
-spec libraries() -> [scm_symbol()].
libraries() ->
    [
     %% , 'base'            @TODO v0.4.0/v0.5.0
     %% , 'case-lambda'     @TODO v0.6.0
     %% , 'char'            @TODO v0.6.0
     %% , 'complex'         @TODO v0.5.0
     %% , 'cxr'             @TODO v0.6.0
     %% , 'file'            @TODO v0.6.0
     %% , 'inexact'         @TODO v0.5.0
     %% , 'lazy'            @TODO v0.6.0
     %% , 'load'            @TODO v0.6.0
     %% , 'process-context' @TODO v0.6.0
     %% , 'read'            @TODO v0.6.0
     %% , 'repl'            @TODO v0.6.0
     %% , 'time'            @TODO v0.6.0
     %% , 'write'           @TODO v0.6.0
     %% , 'r5rs'            @TODO v0.6.0
    ].

-spec list_part(scm_list(), scm_start()) -> scm_list().
list_part(X, Start) when Start >= 0 ->
    list_part1(X, Start);
list_part(X, Start) ->
    erlang:error(badarg, [X, Start]).

-spec list_part(scm_list(), scm_start(), scm_end()) -> scm_list().
list_part(X, Start, End) when Start >= 0, End-Start >= 0 ->
    list_part2(list_part1(X, Start), End - Start);
list_part(X, Start, End) ->
    erlang:error(badarg, [X, Start, End]).

-spec tuple_part(tuple(), scm_start()) -> tuple().
tuple_part(X, Start) when Start >= 0 ->
    tuple_part(X, Start, tuple_size(X));
tuple_part(X, Start) ->
    erlang:error(badarg, [X, Start]).

-spec tuple_part(tuple(), scm_start(), scm_end()) -> tuple().
tuple_part(X, Start, End) when Start >= 0, End-Start >= 0 ->
    tuple_part1(X, Start, End-Start);
tuple_part(X, Start, End) ->
    erlang:error(badarg, [X, Start, End]).

-spec binary_part(binary(), scm_start()) -> binary().
binary_part(X, Start) when Start >= 0 ->
    binary_part(X, Start, byte_size(X));
binary_part(X, Start) ->
    erlang:error(badarg, [X, Start]).

-spec binary_part(binary(), scm_start(), scm_end()) -> binary().
binary_part(X, Start, End) when Start >= 0, End-Start >= 0 ->
    binary_part1(X, Start, End-Start);
binary_part(X, Start, End) ->
    erlang:error(badarg, [X, Start, End]).

-spec to_unicode(iodata()) -> [unichar()].
to_unicode(X) when is_binary(X) ->
    to_unicode(X, utf8);
to_unicode(X) ->
    to_unicode(X, unicode).

-spec to_utf8(iodata()) -> utf8().
to_utf8(X) when is_binary(X) ->
    to_utf8(X, utf8);
to_utf8(X) ->
    to_utf8(X, unicode).

-spec unicode_to_utf8([unichar()]) -> utf8().
unicode_to_utf8(X) ->
    to_utf8(X).

-spec unicode_to_utf8([unichar()], scm_start()) -> utf8().
unicode_to_utf8(X, Start) ->
    to_utf8(list_part(X, Start)).

-spec unicode_to_utf8([unichar()], scm_start(), scm_end()) -> utf8().
unicode_to_utf8(X, Start, End) ->
    to_utf8(list_part(X, Start, End)).

-spec utf8_to_unicode(utf8()) -> [unichar()].
utf8_to_unicode(X) ->
    to_unicode(X).

-spec utf8_to_unicode(utf8(), scm_start()) -> [unichar()].
utf8_to_unicode(X, Start) ->
    list_part(to_unicode(X), Start).

-spec utf8_to_unicode(utf8(), scm_start(), scm_end()) -> [unichar()].
utf8_to_unicode(X, Start, End) ->
    list_part(to_unicode(X), Start, End).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

list_part1([_|T], 1) ->
    T;
list_part1([_|T], N) when N > 1 ->
    list_part1(T, N - 1);
list_part1(L, 0) when is_list(L) ->
    L;
list_part1(L, N) ->
    erlang:error(badarg, [L, N]).

list_part2([H|T], Len) when Len > 0 ->
    [H|list_part2(T, Len-1)];
list_part2(_, 0) ->
    [];
list_part2(X, Len) when is_list(X), Len > 0 ->
    erlang:error(badarg, [X, Len]).

tuple_part1(X, Pos, Len) ->
    %% @TODO This needs profiling vs. make_tuple(Len, undefined)
    list_to_tuple(list_part2(list_part(tuple_to_list(X), Pos), Len)).

binary_part1(X, Pos, Len) ->
    binary:part(X, Pos, Len).

to_unicode(Data, InEncoding) ->
    case unicode:characters_to_list(Data, InEncoding) of
        L when is_list(L) ->
            L;
        Err ->
            erlang:error(Err)
    end.

to_utf8(Data, InEncoding) ->
    case unicode:characters_to_binary(Data, InEncoding, utf8) of
        B when is_binary(B) ->
            B;
        Err ->
            erlang:error(Err)
    end.
