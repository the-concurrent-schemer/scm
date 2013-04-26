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

%%% @doc Scheme base library for strings
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_base_string).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['string?'/1
         , 'make-string'/1
         , 'make-string'/2
         , 'string'/1
         , 'string-length'/1
         , 'string-ref'/2
         , 'string-set!'/3
         , 'string=?'/1
         , 'string<?'/1
         , 'string>?'/1
         , 'string<=?'/1
         , 'string>=?'/1
         , 'substring'/3
         , 'string-append'/1
         , 'string->list'/1
         , 'string->list'/2
         , 'string->list'/3
         , 'list->string'/1
         , 'string-copy'/1
         , 'string-copy'/2
         , 'string-copy'/3
         , 'string-copy!'/3
         , 'string-copy!'/4
         , 'string-copy!'/5
         , 'string-fill!'/2
         , 'string-fill!'/3
         , 'string-fill!'/4
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
    [{'string?', #nipn{val=fun 'string?'/1}}
     , {'make-string', #nipn{val=[fun 'make-string'/1, fun 'make-string'/2]}}
     , {'string', #nipn{val=fun 'string'/1}}
     , {'string-length', #nipn{val=fun 'string-length'/1}}
     , {'string-ref', #nipn{val=fun 'string-ref'/2}}
     , {'string-set!', #nipn{val=fun 'string-set!'/3}}
     , {'string=?', #nipv{val=fun 'string=?'/1}}
     , {'string<?', #nipv{val=fun 'string<?'/1}}
     , {'string>?', #nipv{val=fun 'string>?'/1}}
     , {'string<=?', #nipv{val=fun 'string<=?'/1}}
     , {'string>=?', #nipv{val=fun 'string>=?'/1}}
     , {'substring', #nipn{val=fun 'substring'/3}}
     , {'string-append', #nipv{val=fun 'string-append'/1}}
     , {'string->list', #nipn{val=[fun 'string->list'/1, fun 'string->list'/2, fun 'string->list'/3]}}
     , {'list->string', #nipn{val=fun 'list->string'/1}}
     , {'string-copy', #nipn{val=[fun 'string-copy'/1, fun 'string-copy'/2, fun 'string-copy'/3]}}
     , {'string-copy!', #nipn{val=[fun 'string-copy!'/3, fun 'string-copy!'/4, fun 'string-copy!'/5]}}
     , {'string-fill!', #nipn{val=[fun 'string-fill!'/2, fun 'string-fill!'/3, fun 'string-fill!'/4]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'string?'(scm_obj()) -> scm_boolean().
'string?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'make-string'(scm_k()) -> scm_string().
'make-string'(K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'make-string'(scm_k(), scm_char()) -> scm_string().
'make-string'(K, C) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K, C]).

-spec 'string'([scm_char(),...]) -> scm_string().
'string'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

%% @doc Returns the number of characters in the given string.
-spec 'string-length'(scm_string()) -> scm_k().
'string-length'(#string{val=S}) ->
    tuple_size(S).

%% @doc Returns character k of string using zero-origin indexing.  It
%% is an error if k is not a valid index of string.
-spec 'string-ref'(scm_string(), scm_k()) -> scm_char().
'string-ref'(#string{val=S}, K) ->
    #character{val=element(K+1, S)}.

-spec 'string-set!'(scm_string(), scm_k(), scm_char()) -> scm_false().
'string-set!'(S, K, C) ->
    erlang:error(unsupported, [S, K, C]).

-spec 'string=?'([scm_string(),...]) -> scm_boolean().
'string=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'string<?'([scm_string(),...]) -> scm_boolean().
'string<?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'string>?'([scm_string(),...]) -> scm_boolean().
'string>?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'string<=?'([scm_string(),...]) -> scm_boolean().
'string<=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'string>=?'([scm_string(),...]) -> scm_boolean().
'string>=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'substring'(scm_string(), scm_start(), scm_end()) -> scm_char().
'substring'(S, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start, End]).

-spec 'string-append'([scm_string(),...]) -> scm_string().
'string-append'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

%% @equiv 'string->list'(S, 0, 'string-length'(S))
-spec 'string->list'(scm_string()) -> [scm_char()].
'string->list'(S) ->
    'string->list'(S, 0, 'string-length'(S)).

%% @equiv 'string->list'(S, Start, 'string-length'(S))
-spec 'string->list'(scm_string(), scm_start()) -> [scm_char()].
'string->list'(S, Start) ->
    'string->list'(S, Start, 'string-length'(S)).

%% @doc Returns a list of the characters of string between start and
%% end.
-spec 'string->list'(scm_string(), scm_start(), scm_end()) -> [scm_char()].
'string->list'(#string{val=S}, Start, End) ->
    [ #character{val=C} || C <- lists:sublist(tuple_to_list(S), Start+1, End-Start) ].

%% @doc Returns a string constructed from the characters in the list.
-spec 'list->string'([scm_char()]) -> scm_string().
'list->string'(Cs) ->
    #string{val=list_to_tuple([ C || #character{val=C} <- Cs ])}.

-spec 'string-copy'(scm_string()) -> scm_string().
'string-copy'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'string-copy'(scm_string(), scm_start()) -> scm_string().
'string-copy'(S, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start]).

-spec 'string-copy'(scm_string(), scm_start(), scm_end()) -> scm_string().
'string-copy'(S, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Start, End]).

-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string()) -> scm_false().
'string-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string(), scm_start()) -> scm_false().
'string-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string(), scm_start(), scm_end()) -> scm_false().
'string-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

-spec 'string-fill!'(scm_string(), scm_char()) -> scm_false().
'string-fill!'(S, Fill) ->
    erlang:error(unsupported, [S, Fill]).

-spec 'string-fill!'(scm_string(), scm_char(), scm_start()) -> scm_false().
'string-fill!'(S, Fill, Start) ->
    erlang:error(unsupported, [S, Fill, Start]).

-spec 'string-fill!'(scm_string(), scm_char(), scm_start(), scm_end()) -> scm_false().
'string-fill!'(S, Fill, Start, End) ->
    erlang:error(unsupported, [S, Fill, Start, End]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
