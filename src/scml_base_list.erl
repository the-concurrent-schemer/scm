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

%%% @doc Scheme base library for pairs and lists
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_list).

%% SCML Exports
-export(['$scml_exports'/0]).

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
         , 'memq'/5
         , 'memv'/5
         , 'member'/5
         , 'member'/6
         , 'assq'/5
         , 'assv'/5
         , 'assoc'/5
         , 'assoc'/6
         , 'list-copy'/1
        ]).

-import(scmi_analyze_primitive, [apply/5]).
-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
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
     , {'list', #nipv{val=fun 'list'/1}}
     , {'length', #nipn{val=fun 'length'/1}}
     , {'append', #nipv{val=fun 'append'/1}}
     , {'reverse', #nipn{val=fun 'reverse'/1}}
     , {'list-tail', #nipn{val=fun 'list-tail'/2}}
     , {'list-ref', #nipn{val=fun 'list-ref'/2}}
     , {'list-set!', #nipn{val=fun 'list-set!'/3}}
     , {'memq', #xnipn{val=fun 'memq'/5}}
     , {'memv', #xnipn{val=fun 'memv'/5}}
     , {'member', #xnipn{val=[fun 'member'/5, fun 'member'/6]}}
     , {'assq', #xnipn{val=fun 'assq'/5}}
     , {'assv', #xnipn{val=fun 'assv'/5}}
     , {'assoc', #xnipn{val=[fun 'assoc'/5, fun 'assoc'/6]}}
     , {'list-copy', #nipv{val=fun 'list-copy'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a pair, and otherwise returns #f.
-spec 'pair?'(scm_obj()) -> scm_boolean().
'pair?'([_|_]) ->
    ?TRUE;
'pair?'(_) ->
    ?FALSE.

%% @doc Returns a pair whose car is obj1 and whose cdr is obj2.  The
%% pair constructed by +cons+ is not guaranteed to be different (in
%% the sense of +eqv?+) from every existing object.
-spec 'cons'(scm_obj(), scm_obj()) -> scm_pair().
'cons'(Obj1, Obj2) ->
    [Obj1|Obj2].

%% @doc Returns the contents of the car field of pair.  Note that it
%% is an error to take the car of the empty list.
-spec 'car'(scm_pair()) -> scm_obj().
'car'([H|_]) ->
    H.

%% @doc Returns the contents of the cdr field of pair.  Note that it
%% is an error to take the car of the empty list.
-spec 'cdr'(scm_pair()) -> scm_obj().
'cdr'([_|T]) ->
    T.

%% @doc _unsupported_
-spec 'set-car!'(scm_pair(), scm_obj()) -> scm_false().
'set-car!'(Pair, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [Pair, Obj]).

%% @doc _unsupported_
-spec 'set-cdr!'(scm_pair(), scm_obj()) -> scm_false().
'set-cdr!'(Pair, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [Pair, Obj]).

%% @doc Returns the composition of car.
-spec 'caar'(scm_pair()) -> scm_obj().
'caar'(Pair) ->
    'car'('car'(Pair)).

%% @doc Returns the composition of car and cdr.
-spec 'cadr'(scm_pair()) -> scm_obj().
'cadr'(Pair) ->
    'car'('cdr'(Pair)).

%% @doc Returns the composition of cdr and car.
-spec 'cdar'(scm_pair()) -> scm_obj().
'cdar'(Pair) ->
    'cdr'('car'(Pair)).

%% @doc Returns the composition of cdr and cdr.
-spec 'cddr'(scm_pair()) -> scm_obj().
'cddr'(Pair) ->
    'cdr'('cdr'(Pair)).

%% @doc Returns #t if obj is the empty list, otherwise returns #f.
-spec 'null?'(scm_obj()) -> scm_boolean().
'null?'([]) ->
    ?TRUE;
'null?'(_) ->
    ?FALSE.

%% @doc Returns #t if obj is a (proper) list, otherwise returns #f.
-spec 'list?'(scm_obj()) -> scm_boolean().
'list?'([]) ->
    ?TRUE;
'list?'([_|T]) ->
    'list?'(T);
'list?'(_) ->
    ?FALSE.

%% @equiv 'make-list'(K, '#f')
-spec 'make-list'(scm_k()) -> scm_list().
'make-list'(K) ->
    'make-list'(K, ?FALSE).

%% @doc Returns a list of k elements.
-spec 'make-list'(scm_k(), scm_obj()) -> scm_list().
'make-list'(K, Fill) ->
    lists:duplicate(K, Fill).

%% @doc Returns a list of the arguments.
-spec 'list'(scm_list()) -> scm_list().
'list'(List) ->
    List.

%% @doc Returns the length of list.
-spec 'length'(scm_list()) -> scm_k().
'length'(List) ->
    erlang:length(List).

%% @doc Returns a list consisting of the elements of the first list
%% followed by the elements of the other lists.  If there are no
%% arguments, the empty list is returned.  If there is exactly one
%% argument, it is returned.  An improper list results if the last
%% argument is not a proper list.  The last argument, if there is one,
%% can be of any type.
-spec 'append'(scmi_vargs()) -> scm_list().
'append'(Lists) ->
    lists:append(Lists).

%% @doc Returns a list consisting of the elements of list in reverse
%% order.
-spec 'reverse'(scm_list()) -> scm_list().
'reverse'(List) ->
    lists:reverse(List).

%% @doc Returns the sublist of list obtained by omitting the first k
%% elements.  It is an error if list has fewer than k elements.
-spec 'list-tail'(scm_list(), scm_k()) -> scm_list().
'list-tail'(List, K) ->
    lists:nthtail(K, List).

%% @doc Returns the kth element of list.  It is an error if list has
%% fewer than k elements.
-spec 'list-ref'(scm_list(), scm_k()) -> scm_obj().
'list-ref'(List, K) ->
    lists:nth(K, List).

%% @doc _unsupported_
-spec 'list-set!'(scm_list(), scm_k(), scm_obj()) -> scm_false().
'list-set!'(List, K, Obj) ->
    %% (nearly?) impossible with the Erlang VM
    erlang:error(unsupported, [List, K, Obj]).

%% @equiv 'memq'(Obj, List, 'eq?')
-spec 'memq'(scm_obj(), scm_list(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_list() | scm_false().
'memq'(Obj, List, Env, Ok, Ng) ->
    'member'(Obj, List, #nipn{val=fun scml_base_equality:'eq?'/2}, Env, Ok, Ng).

%% @equiv 'memq'(Obj, List, 'eqv?')
-spec 'memv'(scm_obj(), scm_list(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_list() | scm_false().
'memv'(Obj, List, Env, Ok, Ng) ->
    'member'(Obj, List, #nipn{val=fun scml_base_equality:'eqv?'/2}, Env, Ok, Ng).

%% @equiv 'memq'(Obj, List, 'equal?')
-spec 'member'(scm_obj(), scm_list(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_list() | scm_false().
'member'(Obj, List, Env, Ok, Ng) ->
    'member'(Obj, List, #nipn{val=fun scml_base_equality:'equal?'/2}, Env, Ok, Ng).

%% @doc Return the first sublist of list whose car is obj where the
%% sublists of list are the non-empty lists returned by (list-tail
%% list k) for k less than the length of list.  If obj does not occur
%% in the list, the #f is returned.
-spec 'member'(scm_obj(), scm_list(), scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_list() | scm_false().
'member'(_Obj, [], _Compare, _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
'member'(Obj, [H|T]=List, Compare, Env, Ok, Ng) ->
    Ok1 = fun(?FALSE, Ng1) ->
                  'member'(Obj, T, Compare, Env, Ok, Ng1);
             (_, Ng1) ->
                  Ok(List, Ng1)
          end,
    apply(Compare, [Obj, H], Env, Ok1, Ng).

%% @equiv 'assq'(Obj, List, 'eq?')
-spec 'assq'(scm_obj(), scm_alist(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_pair() | scm_false().
'assq'(Obj, Alist, Env, Ok, Ng) ->
    'assoc'(Obj, Alist, #nipn{val=fun scml_base_equality:'eq?'/2}, Env, Ok, Ng).

%% @equiv 'assv'(Obj, List, 'eqv?')
-spec 'assv'(scm_obj(), scm_alist(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_pair() | scm_false().
'assv'(Obj, Alist, Env, Ok, Ng) ->
    'assoc'(Obj, Alist, #nipn{val=fun scml_base_equality:'eqv?'/2}, Env, Ok, Ng).

%% @equiv 'assoc'(Obj, List, 'equal?')
-spec 'assoc'(scm_obj(), scm_alist(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_pair() | scm_false().
'assoc'(Obj, Alist, Env, Ok, Ng) ->
    'assoc'(Obj, Alist, #nipn{val=fun scml_base_equality:'equal?'/2}, Env, Ok, Ng).

%% @doc Return the first pair of alist whose car field is obj.  If no
%% pair in alist has obj as its car, then #f is returned.
-spec 'assoc'(scm_obj(), scm_alist(), scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_pair() | scm_false().
'assoc'(_Obj, [], _Compare, _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
'assoc'(Obj, [[H|_]=Pair|T], Compare, Env, Ok, Ng) ->
    Ok1 = fun(?FALSE, Ng1) ->
                  'assoc'(Obj, T, Compare, Env, Ok, Ng1);
             (_, Ng1) ->
                  Ok(Pair, Ng1)
          end,
    apply(Compare, [Obj, H], Env, Ok1, Ng).

%% @equiv list(obj)
-spec 'list-copy'(scm_obj()) -> scm_obj().
'list-copy'(Obj) ->
    Obj.
