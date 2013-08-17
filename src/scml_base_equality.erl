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

%%% @doc Scheme base library for equivalence predicates
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

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
    [{'eqv?', #nipn{val=fun ?MODULE:'eqv?'/2}}
     , {'eq?', #nipn{val=fun ?MODULE:'eq?'/2}}
     , {'equal?', #nipn{val=fun ?MODULE:'equal?'/2}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj1 and obj2 are normally regarded as the same
%% object.  Otherwise, #f.
-spec 'eqv?'(scm_obj(), scm_obj()) -> scm_boolean().
'eqv?'(#boolean{val=A}, #boolean{val=A}) ->
    ?TRUE;
'eqv?'(A, A) when is_atom(A) ->
    ?TRUE;
'eqv?'({SHA, A}, {SHA, A}) when is_atom(SHA), is_binary(A) ->
    ?TRUE;
'eqv?'(A, B) when is_number(A); is_number(B) ->
    'eqv-number?'(A, B);
'eqv?'(A, B) when tuple_size(A)==2; tuple_size(B)==2 ->
    'eqv-number?'(A, B);
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
'eqv?'(#label{val={C,A}}, #label{val={C,B}}) ->
    'eqv?'(A, B);
'eqv?'(#labelref{val=A}, #labelref{val=A}) ->
    ?TRUE;
'eqv?'(A, A) when is_pid(A) ->
    ?TRUE;
'eqv?'(#nip0{val=A}, #nip0{val=A}) ->
    ?TRUE;
'eqv?'(#nipn{val=A}, #nipn{val=A}) ->
    ?TRUE;
'eqv?'(#nipv{val=A}, #nipv{val=A}) ->
    ?TRUE;
'eqv?'(#nipnv{val=A}, #nipnv{val=A}) ->
    ?TRUE;
'eqv?'(#xnip0{val=A}, #xnip0{val=A}) ->
    ?TRUE;
'eqv?'(#xnipn{val=A}, #xnipn{val=A}) ->
    ?TRUE;
'eqv?'(#xnipv{val=A}, #xnipv{val=A}) ->
    ?TRUE;
'eqv?'(#xnipnv{val=A}, #xnipnv{val=A}) ->
    ?TRUE;
'eqv?'(#lip0{val=A}, #lip0{val=A}) ->
    ?TRUE;
'eqv?'(#lipn{val=A}, #lipn{val=A}) ->
    ?TRUE;
'eqv?'(#lipv{val=A}, #lipv{val=A}) ->
    ?TRUE;
'eqv?'(#lipnv{val=A}, #lipnv{val=A}) ->
    ?TRUE;
'eqv?'([A1|A2], [B1|B2]) ->
    case 'eqv?'(A1, B1) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'eqv?'(A2, B2)
    end;
'eqv?'(_, _) ->
    ?FALSE.

%% @equiv 'eqv?'/2
-spec 'eq?'(scm_obj(), scm_obj()) -> scm_boolean().
'eq?'(A, B) ->
    'eqv?'(A, B).

%% @doc Returns #t if obj1 and obj2 have the same display
%% representation.  Otherwise, #f.
-spec 'equal?'(scm_obj(), scm_obj()) -> scm_boolean().
'equal?'(A, B) ->
    case 'eqv?'(A, B) of
        ?FALSE ->
            %% Recursively call equal? on the arguments and body
            %% sources of lambda-based procedures
            case {A, B} of
                {#lip0{val=#l0{src=X}}, #lip0{val=#l0{src=Y}}} ->
                    'equal-lambda?'(X, Y);
                {#lipn{val=#ln{params=I, src=X}}, #lipn{val=#ln{params=J, src=Y}}} ->
                    'equal-lambda?'(I, X, J, Y);
                {#lipv{val=#lv{param=I, src=X}}, #lipv{val=#lv{param=J, src=Y}}} ->
                    'equal-lambda?'(I, X, J, Y);
                {#lipnv{val=#lnv{n=N, params=I, src=X}}, #lipnv{val=#lnv{n=N, params=J, src=Y}}} ->
                    'equal-lambda?'(I, X, J, Y);
                _ ->
                    ?FALSE
            end;
        _ ->
            ?TRUE
    end.

%%%===================================================================
%%% internal helpers
%%%===================================================================

'eqv-number?'(A, B) ->
    case {scml_base_number:'exact?'(A), scml_base_number:'exact?'(B)} of
        {X, X} ->
            scml_base_number:'='([A, B]);
        _ ->
            ?FALSE
    end.

'equal-lambda?'(SrcA, SrcB) ->
    'equal?'(SrcA(), SrcB()).

'equal-lambda?'(ParamsA, SrcA, ParamsB, SrcB) ->
    case 'equal?'(ParamsA, ParamsB) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'equal-lambda?'(SrcA, SrcB)
    end.
