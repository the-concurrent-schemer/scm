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

-module(scml_base_equality).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['eqv?'/2
         , 'eq?'/2
         , 'equal?'/2
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
    [{'eqv?', #nipn{val=fun 'eqv?'/2}}
     , {'eq?', #nipn{val=fun 'eq?'/2}}
     , {'equal?', #nipn{val=fun 'equal?'/2}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc This function is a temporary place holder and is not (yet)
%% compliant with the R7RS specification.
%% @TODO 'v0.4.0'
-spec 'eqv?'(scm_obj(), scm_obj()) -> scm_boolean().
'eqv?'(?TRUE, ?TRUE) ->
    ?TRUE;
'eqv?'(?FALSE, ?FALSE) ->
    ?TRUE;
'eqv?'(A, A) when is_atom(A) ->
    ?TRUE;
'eqv?'({Sha, A}, {Sha, A}) when is_atom(Sha), is_binary(A) ->
    ?TRUE;
'eqv?'(A, B) when is_number(A), is_number(B) ->
    case A == B of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end;
'eqv?'({A, B}, {C, D}) when is_number(A), is_number(B), is_number(C), is_number(D) ->
    case A == B andalso C == D of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end;
'eqv?'(?PINF, ?PINF) ->
    ?TRUE;
'eqv?'(?NINF, ?NINF) ->
    ?TRUE;
'eqv?'(?NZER, ?NZER) ->
    ?TRUE;
'eqv?'(A, ?NZER) when A == 0 ->
    ?TRUE;
'eqv?'(?NZER, B) when B == 0 ->
    ?TRUE;
'eqv?'({Complex, {A, B}}, {Complex, {C, D}}) when Complex==rectangular; Complex==polar ->
    case 'eqv?'(A, C) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'eqv?'(B, D)
    end;
'eqv?'(#character{val=A}, #character{val=A}) ->
    ?TRUE;
'eqv?'([], []) ->
    ?TRUE;
'eqv?'(#vector{val=A}, #vector{val=B}) ->
    'eqv?'(tuple_to_list(A), tuple_to_list(B));
'eqv?'(#bytevector{val=A}, #bytevector{val=A}) ->
    ?TRUE;
'eqv?'(#string{val=A}, #string{val=A}) ->
    ?TRUE;
'eqv?'([H1|T1], [H2|T2]) ->
    case 'eqv?'(H1, H2) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'eqv?'(T1, T2)
    end;
'eqv?'(_, _) ->
    ?FALSE.

%% @doc This function is a temporary place holder and is not (yet)
%% compliant with the R7RS specification.
%% @TODO 'v0.4.0'
-spec 'eq?'(scm_obj(), scm_obj()) -> scm_boolean().
'eq?'(Obj1, Obj2) ->
    'eqv?'(Obj1, Obj2).

%% @doc This function is a temporary place holder and is not (yet)
%% compliant with the R7RS specification.
%% @TODO 'v0.4.0'
-spec 'equal?'(scm_obj(), scm_obj()) -> scm_boolean().
'equal?'(Obj1, Obj2) ->
    'eq?'(Obj1, Obj2).

%%%===================================================================
%%% internal helpers
%%%===================================================================
