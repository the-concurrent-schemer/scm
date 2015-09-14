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

%%% @doc Scheme base library for vectors
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_base_vector).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['vector?'/1
         , 'make-vector'/1
         , 'make-vector'/2
         , 'vector'/1
         , 'vector-length'/1
         , 'vector-ref'/2
         , 'vector-set!'/3
         , 'vector->list'/1
         , 'vector->list'/2
         , 'vector->list'/3
         , 'list->vector'/1
         , 'vector->string'/1
         , 'vector->string'/2
         , 'vector->string'/3
         , 'string->vector'/1
         , 'string->vector'/2
         , 'string->vector'/3
         , 'vector-copy'/1
         , 'vector-copy'/2
         , 'vector-copy'/3
         , 'vector-copy!'/3
         , 'vector-copy!'/4
         , 'vector-copy!'/5
         , 'vector-append'/1
         , 'vector-fill!'/2
         , 'vector-fill!'/3
         , 'vector-fill!'/4
        ]).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'vector?', #nipn{val=fun ?MODULE:'vector?'/1}}
     , {'make-vector', #nipn{val=[fun ?MODULE:'make-vector'/1, fun ?MODULE:'make-vector'/2]}}
     , {'vector', #nipv{val=fun ?MODULE:'vector'/1}}
     , {'vector-length', #nipn{val=fun ?MODULE:'vector-length'/1}}
     , {'vector-ref', #nipn{val=fun ?MODULE:'vector-ref'/2}}
     , {'vector-set!', #nipn{val=fun ?MODULE:'vector-set!'/3}}
     , {'vector->list', #nipn{val=[fun ?MODULE:'vector->list'/1, fun ?MODULE:'vector->list'/2, fun ?MODULE:'vector->list'/3]}}
     , {'list->vector', #nipn{val=fun ?MODULE:'list->vector'/1}}
     , {'vector->string', #nipn{val=[fun ?MODULE:'vector->string'/1, fun ?MODULE:'vector->string'/2, fun ?MODULE:'vector->string'/3]}}
     , {'string->vector', #nipn{val=[fun ?MODULE:'string->vector'/1, fun ?MODULE:'string->vector'/2, fun ?MODULE:'string->vector'/3]}}
     , {'vector-copy', #nipn{val=[fun ?MODULE:'vector-copy'/1, fun ?MODULE:'vector-copy'/2, fun ?MODULE:'vector-copy'/3]}}
     , {'vector-copy!', #nipn{val=[fun ?MODULE:'vector-copy!'/3, fun ?MODULE:'vector-copy!'/4, fun ?MODULE:'vector-copy!'/5]}}
     , {'vector-append', #nipv{val=fun ?MODULE:'vector-append'/1}}
     , {'vector-fill!', #nipn{val=[fun ?MODULE:'vector-fill!'/2, fun ?MODULE:'vector-fill!'/3, fun ?MODULE:'vector-fill!'/4]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a vector, otherwise returns #f.
-spec 'vector?'(scm_obj()) -> scm_boolean().
'vector?'(#vector{}) ->
    ?TRUE;
'vector?'(_) ->
    ?FALSE.

%% @equiv 'make-vector'(K, '#f')
-spec 'make-vector'(scm_k()) -> scm_vector().
'make-vector'(K) ->
    'make-vector'(K, ?FALSE).

%% @doc Returns a vector of k elements.
-spec 'make-vector'(scm_k(), scm_obj()) -> scm_vector().
'make-vector'(K, Obj) ->
    #vector{val=list_to_tuple(lists:duplicate(K, Obj))}.

%% @doc Returns a vector whose elements contain the given arguments.
-spec 'vector'([scm_obj(),...]) -> scm_vector().
'vector'(Objs) ->
    #vector{val=list_to_tuple(Objs)}.

%% @doc Returns the number of elements in the given vector.
-spec 'vector-length'(scm_vector()) -> scm_k().
'vector-length'(#vector{val=V}) ->
    tuple_size(V).

%% @doc Returns element k of vector using zero-origin indexing.  It is
%% an error if k is not a valid index of vector.
-spec 'vector-ref'(scm_vector(), scm_k()) -> scm_obj().
'vector-ref'(#vector{val=V}, K) ->
    element(K+1, V).

%% @doc _unsupported_
-spec 'vector-set!'(scm_vector(), scm_k(), scm_obj()) -> scm_false().
'vector-set!'(V, K, Obj) ->
    erlang:error(unsupported, [V, K, Obj]).

%% @equiv 'vector->list'(V, 0, 'vector-length'(V))
-spec 'vector->list'(scm_vector()) -> [scm_obj()].
'vector->list'(#vector{val=V}) ->
    tuple_to_list(V).

%% @equiv 'vector->list'(V, Start, 'vector-length'(V))
-spec 'vector->list'(scm_vector(), scm_start()) -> [scm_obj()].
'vector->list'(#vector{val=V}, Start) ->
    scml:list_part(tuple_to_list(V), Start).

%% @doc Returns a list of the elements of vector between start and
%% end.
-spec 'vector->list'(scm_vector(), scm_start(), scm_end()) -> [scm_obj()].
'vector->list'(#vector{val=V}, Start, End) ->
    scml:list_part(tuple_to_list(V), Start, End).

%% @doc Returns a vector constructed from the elements in the list.
-spec 'list->vector'([scm_obj()]) -> scm_vector().
'list->vector'(Objs) ->
    #vector{val=list_to_tuple(Objs)}.

%% @equiv 'vector->string'(V, 0, 'vector-length'(V))
-spec 'vector->string'(scm_vector()) -> scm_string().
'vector->string'(V) ->
    scml_base_string:'list->string'('vector->list'(V)).

%% @equiv 'vector->string'(V, Start, 'vector-length'(V))
-spec 'vector->string'(scm_vector(), scm_start()) -> scm_string().
'vector->string'(V, Start) ->
    scml_base_string:'list->string'('vector->list'(V, Start)).

%% @equiv 'list->string'('vector->list'(V, Start, End))
-spec 'vector->string'(scm_vector(), scm_start(), scm_end()) -> scm_string().
'vector->string'(V, Start, End) ->
    scml_base_string:'list->string'('vector->list'(V, Start, End)).

%% @equiv 'list->vector'('string->list'(S, 0, 'string-length'(S)))
-spec 'string->vector'(scm_string()) -> scm_vector().
'string->vector'(S) ->
    'list->vector'(scml_base_string:'string->list'(S)).

%% @equiv 'list->vector'('string->list'(S, Start, 'string-length'(S)))
-spec 'string->vector'(scm_string(), scm_start()) -> scm_vector().
'string->vector'(S, Start) ->
    'list->vector'(scml_base_string:'string->list'(S, Start)).

%% @equiv 'list->vector'('string->list'(S, Start, End))
-spec 'string->vector'(scm_string(), scm_start(), scm_end()) -> scm_vector().
'string->vector'(S, Start, End) ->
    'list->vector'(scml_base_string:'string->list'(S, Start, End)).

%% @equiv 'vector-copy'(V, 0, 'vector-length'(V))
-spec 'vector-copy'(scm_vector()) -> scm_vector().
'vector-copy'(V) ->
    V.

%% @equiv 'vector-copy'(V, Start, 'vector-length'(V))
-spec 'vector-copy'(scm_vector(), scm_start()) -> scm_vector().
'vector-copy'(#vector{val=V}, Start) ->
    #vector{val=scml:tuple_part(V, Start)}.

%% @doc Returns a vector constructed from the elements of vector
%% beginning with index start and ending with index end.
-spec 'vector-copy'(scm_vector(), scm_start(), scm_end()) -> scm_vector().
'vector-copy'(V, Start, End) ->
    #vector{val=scml:tuple_part(V, Start, End)}.

%% @doc _unsupported_
-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector()) -> scm_false().
'vector-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

%% @doc _unsupported_
-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector(), scm_start()) -> scm_false().
'vector-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

%% @doc _unsupported_
-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector(), scm_start(), scm_end()) -> scm_false().
'vector-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

%% @doc Returns a vector whose elements are the concatenation of the
%% elements in the given vectors.
-spec 'vector-append'([scm_vector(),...]) -> scm_vector().
'vector-append'(Vs) ->
    #vector{val=list_to_tuple(lists:append([ tuple_to_list(V) || V <- Vs ]))}.

%% @doc _unsupported_
-spec 'vector-fill!'(scm_vector(), scm_obj()) -> scm_false().
'vector-fill!'(V, Fill) ->
    erlang:error(unsupported, [V, Fill]).

%% @doc _unsupported_
-spec 'vector-fill!'(scm_vector(), scm_obj(), scm_start()) -> scm_false().
'vector-fill!'(V, Fill, Start) ->
    erlang:error(unsupported, [V, Fill, Start]).

%% @doc _unsupported_
-spec 'vector-fill!'(scm_vector(), scm_obj(), scm_start(), scm_end()) -> scm_false().
'vector-fill!'(V, Fill, Start, End) ->
    erlang:error(unsupported, [V, Fill, Start, End]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
