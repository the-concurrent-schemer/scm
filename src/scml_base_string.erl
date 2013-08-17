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
    [{'string?', #nipn{val=fun ?MODULE:'string?'/1}}
     , {'make-string', #nipn{val=[fun ?MODULE:'make-string'/1, fun ?MODULE:'make-string'/2]}}
     , {'string', #nipn{val=fun ?MODULE:'string'/1}}
     , {'string-length', #nipn{val=fun ?MODULE:'string-length'/1}}
     , {'string-ref', #nipn{val=fun ?MODULE:'string-ref'/2}}
     , {'string-set!', #nipn{val=fun ?MODULE:'string-set!'/3}}
     , {'string=?', #nipv{val=fun ?MODULE:'string=?'/1}}
     , {'string<?', #nipv{val=fun ?MODULE:'string<?'/1}}
     , {'string>?', #nipv{val=fun ?MODULE:'string>?'/1}}
     , {'string<=?', #nipv{val=fun ?MODULE:'string<=?'/1}}
     , {'string>=?', #nipv{val=fun ?MODULE:'string>=?'/1}}
     , {'substring', #nipn{val=fun ?MODULE:'substring'/3}}
     , {'string-append', #nipv{val=fun ?MODULE:'string-append'/1}}
     , {'string->list', #nipn{val=[fun ?MODULE:'string->list'/1, fun ?MODULE:'string->list'/2, fun ?MODULE:'string->list'/3]}}
     , {'list->string', #nipn{val=fun ?MODULE:'list->string'/1}}
     , {'string-copy', #nipn{val=[fun ?MODULE:'string-copy'/1, fun ?MODULE:'string-copy'/2, fun ?MODULE:'string-copy'/3]}}
     , {'string-copy!', #nipn{val=[fun ?MODULE:'string-copy!'/3, fun ?MODULE:'string-copy!'/4, fun ?MODULE:'string-copy!'/5]}}
     , {'string-fill!', #nipn{val=[fun ?MODULE:'string-fill!'/2, fun ?MODULE:'string-fill!'/3, fun ?MODULE:'string-fill!'/4]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a string, otherwise returns #f.
-spec 'string?'(scm_obj()) -> scm_boolean().
'string?'(#string{}) ->
    ?TRUE;
'string?'(_) ->
    ?FALSE.

%% @equiv 'make-string'(K, '#\\null')
-spec 'make-string'(scm_k()) -> scm_string().
'make-string'(K) ->
    'make-string'(K, #character{val=$0}).

%% @doc Returns a string of k characters.
-spec 'make-string'(scm_k(), scm_char()) -> scm_string().
'make-string'(K, #character{val=C}) ->
    #string{val=list_to_tuple(lists:duplicate(K, C))}.

%% @doc Returns a string composed of the arguments.
-spec 'string'([scm_char(),...]) -> scm_string().
'string'(Cs) ->
    #string{val=list_to_tuple([ C || #character{val=C} <- Cs ])}.

%% @doc Returns the number of characters in the given string.
-spec 'string-length'(scm_string()) -> scm_k().
'string-length'(#string{val=S}) ->
    tuple_size(S).

%% @doc Returns character k of string using zero-origin indexing.  It
%% is an error if k is not a valid index of string.
-spec 'string-ref'(scm_string(), scm_k()) -> scm_char().
'string-ref'(#string{val=S}, K) ->
    #character{val=element(K+1, S)}.

%% @doc _unsupported_
-spec 'string-set!'(scm_string(), scm_k(), scm_char()) -> scm_false().
'string-set!'(S, K, C) ->
    erlang:error(unsupported, [S, K, C]).

%% @doc Returns #t if all the strings are the same length and contain
%% exactly the same characters in the same positions, otherwise
%% returns #f.
-spec 'string=?'([scm_string(),...]) -> scm_boolean().
'string=?'(Ss) ->
    cmp(Ss, fun(A, B) -> A =:= B end).

%% @doc Returns #t if all the strings are monotonically increasing,
%% otherwise returns #f.
-spec 'string<?'([scm_string(),...]) -> scm_boolean().
'string<?'(Ss) ->
    cmp(Ss, fun(A, B) -> A < B end).

%% @doc Returns #t if all the strings are monotonically decreasing,
%% otherwise returns #f.
-spec 'string>?'([scm_string(),...]) -> scm_boolean().
'string>?'(Ss) ->
    cmp(Ss, fun(A, B) -> A > B end).

%% @doc Returns #t if all the strings are monotonically
%% non-decreasing, otherwise returns #f.
-spec 'string<=?'([scm_string(),...]) -> scm_boolean().
'string<=?'(Ss) ->
    cmp(Ss, fun(A, B) -> A =< B end).

%% @doc Returns #t if all the strings are monotonically
%% non-increasing, otherwise returns #f.
-spec 'string>=?'([scm_string(),...]) -> scm_boolean().
'string>=?'(Ss) ->
    cmp(Ss, fun(A, B) -> A >= B end).

%% @equiv 'string-copy'(S, Start, End)
-spec 'substring'(scm_string(), scm_start(), scm_end()) -> scm_string().
'substring'(S, Start, End) ->
    'string-copy'(S, Start, End).

%% @doc Returns a string whose characters are the concatenation of the
%% characters in the given strings.
-spec 'string-append'([scm_string(),...]) -> scm_string().
'string-append'(Ss) ->
    #string{val=list_to_tuple(lists:append([ tuple_to_list(S) || S <- Ss ]))}.

%% @equiv 'string->list'(S, 0, 'string-length'(S))
-spec 'string->list'(scm_string()) -> [scm_char()].
'string->list'(#string{val=S}) ->
    [ #character{val=C} || C <- tuple_to_list(S) ].

%% @equiv 'string->list'(S, Start, 'string-length'(S))
-spec 'string->list'(scm_string(), scm_start()) -> [scm_char()].
'string->list'(#string{val=S}, Start) ->
    [ #character{val=C} || C <- scml:list_part(tuple_to_list(S), Start) ].

%% @doc Returns a list of the characters of string between start and
%% end.
-spec 'string->list'(scm_string(), scm_start(), scm_end()) -> [scm_char()].
'string->list'(#string{val=S}, Start, End) ->
    [ #character{val=C} || C <- scml:list_part(tuple_to_list(S), Start, End) ].

%% @doc Returns a string constructed from the characters in the list.
-spec 'list->string'([scm_char()]) -> scm_string().
'list->string'(Cs) ->
    #string{val=list_to_tuple([ C || #character{val=C} <- Cs ])}.

%% @equiv 'string-copy'(S, 0, 'string-length'(S))
-spec 'string-copy'(scm_string()) -> scm_string().
'string-copy'(S) ->
    S.

%% @equiv 'string-copy'(S, Start, 'string-length'(S))
-spec 'string-copy'(scm_string(), scm_start()) -> scm_string().
'string-copy'(#string{val=S}, Start) ->
    #string{val=scml:tuple_part(S, Start)}.

%% @doc Returns a string constructed from the characters of string
%% beginning with index start and ending with index end.
-spec 'string-copy'(scm_string(), scm_start(), scm_end()) -> scm_string().
'string-copy'(S, Start, End) ->
    #string{val=scml:tuple_part(S, Start, End)}.

%% @doc _unsupported_
-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string()) -> scm_false().
'string-copy!'(To, At, From) ->
    erlang:error(unsupported, [To, At, From]).

%% @doc _unsupported_
-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string(), scm_start()) -> scm_false().
'string-copy!'(To, At, From, Start) ->
    erlang:error(unsupported, [To, At, From, Start]).

%% @doc _unsupported_
-spec 'string-copy!'(scm_bytevector(), scm_k(), scm_string(), scm_start(), scm_end()) -> scm_false().
'string-copy!'(To, At, From, Start, End) ->
    erlang:error(unsupported, [To, At, From, Start, End]).

%% @doc _unsupported_
-spec 'string-fill!'(scm_string(), scm_char()) -> scm_false().
'string-fill!'(S, Fill) ->
    erlang:error(unsupported, [S, Fill]).

%% @doc _unsupported_
-spec 'string-fill!'(scm_string(), scm_char(), scm_start()) -> scm_false().
'string-fill!'(S, Fill, Start) ->
    erlang:error(unsupported, [S, Fill, Start]).

%% @doc _unsupported_
-spec 'string-fill!'(scm_string(), scm_char(), scm_start(), scm_end()) -> scm_false().
'string-fill!'(S, Fill, Start, End) ->
    erlang:error(unsupported, [S, Fill, Start, End]).

%%%===================================================================
%%% internal helpers
%%%===================================================================

cmp([], _Fun) ->
    ?TRUE;
cmp([#string{}], _Fun) ->
    ?TRUE;
cmp([#string{val=A}, #string{val=B}=S|Ss], Fun) ->
    case Fun(A, B) of
        true ->
            cmp([S|Ss], Fun);
        false ->
            ?FALSE
    end;
cmp(_, _Fun) ->
    ?FALSE.
