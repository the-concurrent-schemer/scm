%%% The MIT License
%%%
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of stringge, to any person obtaining a copy
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

-module(scml_base_vector).

%% Imports
-export([imports/0]).

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

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'vector?', #nipn{val=fun 'vector?'/1}}
     , {'make-vector', #nipn{val=[fun 'make-vector'/1, fun 'make-vector'/2]}}
     , {'vector', #nipn{val=fun 'vector'/1}}
     , {'vector-length', #nipn{val=fun 'vector-length'/1}}
     , {'vector-ref', #nipn{val=fun 'vector-ref'/2}}
     , {'vector-set!', #nipn{val=fun 'vector-set!'/3}}
     , {'vector->list', #nipn{val=[fun 'vector->list'/1, fun 'vector->list'/2, fun 'vector->list'/3]}}
     , {'list->vector', #nipn{val=fun 'list->vector'/1}}
     , {'vector->string', #nipn{val=[fun 'vector->string'/1, fun 'vector->string'/2, fun 'vector->string'/3]}}
     , {'string->vector', #nipn{val=[fun 'string->vector'/1, fun 'string->vector'/2, fun 'string->vector'/3]}}
     , {'vector-copy', #nipn{val=[fun 'vector-copy'/1, fun 'vector-copy'/2, fun 'vector-copy'/3]}}
     , {'vector-copy!', #nipn{val=[fun 'vector-copy!'/3, fun 'vector-copy!'/4, fun 'vector-copy!'/5]}}
     , {'vector-append', #nipv{val=fun 'vector-append'/1}}
     , {'vector-fill!', #nipn{val=[fun 'vector-fill!'/2, fun 'vector-fill!'/3, fun 'vector-fill!'/4]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'vector?'(scm_obj()) -> scm_boolean().
'vector?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'make-vector'(scm_k()) -> scm_vector().
'make-vector'(K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'make-vector'(scm_k(), scm_obj()) -> scm_vector().
'make-vector'(K, Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K, Obj]).

-spec 'vector'([scm_obj(),...]) -> scm_vector().
'vector'(Objs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Objs]).

-spec 'vector-length'(scm_vector()) -> scm_k().
'vector-length'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'vector-ref'(scm_vector(), scm_k()) -> scm_obj().
'vector-ref'(V, K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, K]).

-spec 'vector-set!'(scm_vector(), scm_k(), scm_obj()) -> scm_false().
'vector-set!'(V, K, Obj) ->
    erlang:error(unsupported, [V, K, Obj]).

-spec 'vector->list'(scm_vector()) -> [scm_obj()].
'vector->list'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'vector->list'(scm_vector(), scm_start()) -> [scm_obj()].
'vector->list'(V, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start]).

-spec 'vector->list'(scm_vector(), scm_start(), scm_end()) -> [scm_obj()].
'vector->list'(V, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start, End]).

-spec 'list->vector'([scm_obj()]) -> scm_vector().
'list->vector'(Objs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Objs]).

-spec 'vector->string'(scm_vector()) -> scm_string().
'vector->string'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'vector->string'(scm_vector(), scm_start()) -> scm_string().
'vector->string'(V, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start]).

-spec 'vector->string'(scm_vector(), scm_start(), scm_end()) -> scm_string().
'vector->string'(V, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start, End]).

-spec 'string->vector'(scm_string()) -> scm_vector().
'string->vector'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'string->vector'(scm_string(), scm_start()) -> scm_vector().
'string->vector'(S, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start]).

-spec 'string->vector'(scm_string(), scm_start(), scm_end()) -> scm_vector().
'string->vector'(S, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start, End]).

-spec 'vector-copy'(scm_vector()) -> scm_vector().
'vector-copy'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'vector-copy'(scm_vector(), scm_start()) -> scm_vector().
'vector-copy'(V, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start]).

-spec 'vector-copy'(scm_vector(), scm_start(), scm_end()) -> scm_vector().
'vector-copy'(V, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Start, End]).

-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector()) -> scm_false().
'vector-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector(), scm_start()) -> scm_false().
'vector-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

-spec 'vector-copy!'(scm_vector(), scm_k(), scm_vector(), scm_start(), scm_end()) -> scm_false().
'vector-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

-spec 'vector-append'([scm_vector(),...]) -> scm_vector().
'vector-append'(Vs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Vs]).

-spec 'vector-fill!'(scm_vector(), scm_obj()) -> scm_false().
'vector-fill!'(V, Fill) ->
    erlang:error(unsupported, [V, Fill]).

-spec 'vector-fill!'(scm_vector(), scm_obj(), scm_start()) -> scm_false().
'vector-fill!'(V, Fill, Start) ->
    erlang:error(unsupported, [V, Fill, Start]).

-spec 'vector-fill!'(scm_vector(), scm_obj(), scm_start(), scm_end()) -> scm_false().
'vector-fill!'(V, Fill, Start, End) ->
    erlang:error(unsupported, [V, Fill, Start, End]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
