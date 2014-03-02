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

%%% @doc Scheme base library for characters
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_base_char).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['char?'/1
         , 'char=?'/1
         , 'char<?'/1
         , 'char>?'/1
         , 'char<=?'/1
         , 'char>=?'/1
         , 'char->integer'/1
         , 'integer->char'/1
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
    [{'char?', #nipn{val=fun ?MODULE:'char?'/1}}
     , {'char=?', #nipv{val=fun ?MODULE:'char=?'/1}}
     , {'char<?', #nipv{val=fun ?MODULE:'char<?'/1}}
     , {'char>?', #nipv{val=fun ?MODULE:'char>?'/1}}
     , {'char<=?', #nipv{val=fun ?MODULE:'char<=?'/1}}
     , {'char>=?', #nipv{val=fun ?MODULE:'char>=?'/1}}
     , {'char->integer', #nipn{val=fun ?MODULE:'char->integer'/1}}
     , {'integer->char', #nipn{val=fun ?MODULE:'integer->char'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a character, otherwise returns #f.
-spec 'char?'(scm_obj()) -> scm_boolean().
'char?'(#character{}) ->
    ?TRUE;
'char?'(_) ->
    ?FALSE.

%% @doc Returns #t if all the characters are equal, otherwise returns
%% #f.
-spec 'char=?'([scm_char(),...]) -> scm_boolean().
'char=?'(Cs) ->
    cmp(Cs, fun(A, B) -> A =:= B end).

%% @doc Returns #t if all the characters are monotonically increasing,
%% otherwise returns #f.
-spec 'char<?'([scm_char(),...]) -> scm_boolean().
'char<?'(Cs) ->
    cmp(Cs, fun(A, B) -> A < B end).

%% @doc Returns #t if all the characters are monotonically decreasing,
%% otherwise returns #f.
-spec 'char>?'([scm_char(),...]) -> scm_boolean().
'char>?'(Cs) ->
    cmp(Cs, fun(A, B) -> A > B end).

%% @doc Returns #t if all the characters are monotonically
%% non-decreasing, otherwise returns #f.
-spec 'char<=?'([scm_char(),...]) -> scm_boolean().
'char<=?'(Cs) ->
    cmp(Cs, fun(A, B) -> A =< B end).

%% @doc Returns #t if all the characters are monotonically
%% non-increasing, otherwise returns #f.
-spec 'char>=?'([scm_char(),...]) -> scm_boolean().
'char>=?'(Cs) ->
    cmp(Cs, fun(A, B) -> A >= B end).

%% @doc Returns an exact integer equal to the Unicode scalar value of
%% the given character.
-spec 'char->integer'(scm_char()) -> scm_n().
'char->integer'(#character{val=C}) ->
    C.

%% @doc Returns a character equal to the Unicode scalar value of the
%% given exact integer.
-spec 'integer->char'(scm_n()) -> scm_char().
'integer->char'(N) ->
    [C] = scml:to_unicode([N]),
    #character{val=C}.

%%%===================================================================
%%% internal helpers
%%%===================================================================

cmp([], _Fun) ->
    ?TRUE;
cmp([#character{}], _Fun) ->
    ?TRUE;
cmp([#character{val=A}, #character{val=B}=S|Ss], Fun) ->
    case Fun(A, B) of
        true ->
            cmp([S|Ss], Fun);
        false ->
            ?FALSE
    end;
cmp(_, _Fun) ->
    ?FALSE.
