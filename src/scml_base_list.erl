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

-module(scml_base_list).

%% Imports
-export([imports/0]).

%% API
-export(['pair?'/1
         , 'cons'/2
         , 'car'/1
         , 'cdr'/1
         , 'set-car!'/2
         , 'set-cdr!'/2
         , 'caar'/1
         , 'cadr'/1
         , 'cdar'/1
         , 'cddr'/1
         , 'null?'/1
         , 'list?'/1
         , 'make-list'/1
         , 'make-list'/2
         , 'list'/1
         , 'length'/1
         , 'append'/1
         , 'reverse'/1
         , 'list-tail'/2
         , 'list-ref'/2
         , 'list-set!'/3
         , 'memq'/2
         , 'memv'/2
         , 'member'/2
         , 'member'/3
         , 'assq'/2
         , 'assv'/2
         , 'assoc'/2
         , 'assoc'/3
         , 'list-copy'/1
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'pair?', #nipn{val=fun 'pair?'/1}}
     , {'cons', #nipn{val=fun 'cons'/2}}
     , {'car', #nipn{val=fun 'car'/1}}
     , {'cdr', #nipn{val=fun 'cdr'/1}}
     , {'set-car!', #nipn{val=fun 'set-car!'/2}}
     , {'set-cdr!', #nipn{val=fun 'set-cdr!'/2}}
     , {'caar', #nipn{val=fun 'caar'/1}}
     , {'cadr', #nipn{val=fun 'cadr'/1}}
     , {'cdar', #nipn{val=fun 'cdar'/1}}
     , {'cddr', #nipn{val=fun 'cddr'/1}}
     , {'null?', #nipn{val=fun 'null?'/1}}
     , {'list?', #nipn{val=fun 'list?'/1}}
     , {'make-list', #nipn{val=[fun 'make-list'/1, fun 'make-list'/2]}}
     , {'list', #nipn{val=fun 'list'/1}}
     , {'length', #nipn{val=fun 'length'/1}}
     , {'append', #nipv{val=fun 'append'/1}}
     , {'reverse', #nipn{val=fun 'reverse'/1}}
     , {'list-tail', #nipn{val=fun 'list-tail'/2}}
     , {'list-ref', #nipn{val=fun 'list-ref'/2}}
     , {'list-set!', #nipn{val=fun 'list-set!'/3}}
     , {'memq', #nipn{val=fun 'memq'/2}}
     , {'memv', #nipn{val=fun 'memv'/2}}
     , {'member', #nipn{val=[fun 'member'/2, fun 'member'/3]}}
     , {'assq', #nipn{val=fun 'assq'/2}}
     , {'assv', #nipn{val=fun 'assv'/2}}
     , {'assoc', #nipn{val=[fun 'assoc'/2, fun 'assoc'/3]}}
     , {'list-copy', #nipn{val=fun 'list-copy'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'pair?'(scm_obj()) -> scm_boolean().
'pair?'([_|_]) ->
    ?TRUE;
'pair?'(_) ->
    ?FALSE.

-spec 'cons'(scm_obj(), scm_obj()) -> scm_pair().
'cons'(Obj1, Obj2) ->
    [Obj1|Obj2].

-spec 'car'(scm_pair()) -> scm_obj().
'car'([H|_]) ->
    H.

-spec 'cdr'(scm_pair()) -> scm_obj().
'cdr'([_|T]) ->
    T.

-spec 'set-car!'(scm_pair(), scm_obj()) -> scm_false().
'set-car!'(Pair, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [Pair, Obj]).

-spec 'set-cdr!'(scm_pair(), scm_obj()) -> scm_false().
'set-cdr!'(Pair, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [Pair, Obj]).

-spec 'caar'(scm_pair()) -> scm_obj().
'caar'(Pair) ->
    'car'('car'(Pair)).

-spec 'cadr'(scm_pair()) -> scm_obj().
'cadr'(Pair) ->
    'car'('cdr'(Pair)).

-spec 'cdar'(scm_pair()) -> scm_obj().
'cdar'(Pair) ->
    'cdr'('car'(Pair)).

-spec 'cddr'(scm_pair()) -> scm_obj().
'cddr'(Pair) ->
    'cdr'('cdr'(Pair)).

-spec 'null?'(scm_obj()) -> scm_boolean().
'null?'([]) ->
    ?TRUE;
'null?'(_) ->
    ?FALSE.

-spec 'list?'(scm_obj()) -> scm_boolean().
'list?'([]) ->
    ?TRUE;
'list?'([_|T]) ->
    'list?'(T);
'list?'(_) ->
    ?FALSE.

-spec 'make-list'(scm_k()) -> scm_list().
'make-list'(K) ->
    'make-list'(K, ?FALSE).

-spec 'make-list'(scm_k(), scm_obj()) -> scm_list().
'make-list'(K, Fill) ->
    lists:duplicate(K, Fill).

-spec 'list'(scm_list()) -> scm_list().
'list'(List) ->
    List.

-spec 'length'(scm_list()) -> scm_k().
'length'(List) ->
    erlang:length(List).

-spec 'append'(scmi_vargs()) -> scm_list().
'append'(Lists) ->
    lists:append(Lists).

-spec 'reverse'(scm_list()) -> scm_list().
'reverse'(List) ->
    lists:reverse(List).

-spec 'list-tail'(scm_list(), scm_k()) -> scm_list().
'list-tail'(List, K) ->
    lists:nthtail(K, List).

-spec 'list-ref'(scm_list(), scm_k()) -> scm_obj().
'list-ref'(List, K) ->
    lists:nth(K, List).

-spec 'list-set!'(scm_list(), scm_k(), scm_obj()) -> scm_false().
'list-set!'(List, K, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [List, K, Obj]).

-spec 'memq'(scm_obj(), scm_list()) -> scm_list() | scm_false().
'memq'(Obj, List) ->
    'member'(Obj, List, fun scml_base_equality:'eq?'/2).

-spec 'memv'(scm_obj(), scm_list()) -> scm_list() | scm_false().
'memv'(Obj, List) ->
    'member'(Obj, List, fun scml_base_equality:'eqv?'/2).

-spec 'member'(scm_obj(), scm_list()) -> scm_list() | scm_false().
'member'(Obj, List) ->
    'member'(Obj, List, fun scml_base_equality:'equal?'/2).

-spec 'member'(scm_obj(), scm_list(), scm_proc()) -> scm_list() | scm_false().
'member'(_Obj, [], _Compare) ->
    ?FALSE;
'member'(Obj, [H|T]=List, Compare) ->
    case Compare(Obj, H) of
        ?FALSE ->
            'member'(Obj, T, Compare);
        _ ->
            List
    end.

-spec 'assq'(scm_obj(), scm_alist()) -> scm_pair() | scm_false().
'assq'(Obj, Alist) ->
    'assoc'(Obj, Alist, fun scml_base_equality:'eq?'/2).

-spec 'assv'(scm_obj(), scm_alist()) -> scm_pair() | scm_false().
'assv'(Obj, Alist) ->
    'assoc'(Obj, Alist, fun scml_base_equality:'eqv?'/2).

-spec 'assoc'(scm_obj(), scm_alist()) -> scm_pair() | scm_false().
'assoc'(Obj, Alist) ->
    'assoc'(Obj, Alist, fun scml_base_equality:'equal?'/2).

-spec 'assoc'(scm_obj(), scm_alist(), scm_proc()) -> scm_pair() | scm_false().
'assoc'(_Obj, [], _Compare) ->
    ?FALSE;
'assoc'(Obj, [[H|_]|T]=Alist, Compare) ->
    case Compare(Obj, H) of
        ?FALSE ->
            'assoc'(Obj, T, Compare);
        _ ->
            Alist
    end.

-spec 'list-copy'(scm_obj()) -> scm_obj().
'list-copy'(Obj) ->
    Obj.
