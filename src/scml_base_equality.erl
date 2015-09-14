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
'eqv?'(#boolean{}=O1, #boolean{}=O2) ->
    scml_base_boolean:'boolean=?'([O1, O2]);
'eqv?'(O1, O2) when is_atom(O1) ->
    scml_base_symbol:'symbol=?'([O1, O2]);
'eqv?'({SHA, A}=O1, O2) when is_atom(SHA), is_binary(A) ->
    scml_base_symbol:'symbol=?'([O1, O2]);
'eqv?'(O1, O2) when is_number(O1) ->
    'number=?'(O1, O2);
'eqv?'(O1, O2) when tuple_size(O1)==2 ->
    'number=?'(O1, O2);
'eqv?'(#character{}=O1, #character{}=O2) ->
    scml_base_char:'char=?'([O1, O2]);
'eqv?'([], []) ->
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
'eqv?'(_, _) ->
    ?FALSE.

%% @equiv 'eqv?'/2
-spec 'eq?'(scm_obj(), scm_obj()) -> scm_boolean().
'eq?'(O1, O2) ->
    'eqv?'(O1, O2).

%% @doc Returns #t if obj1 and obj2 have the same display
%% representation.  Otherwise, #f.
-spec 'equal?'(scm_obj(), scm_obj()) -> scm_boolean().
'equal?'(#boolean{}=O1, #boolean{}=O2) ->
    scml_base_boolean:'boolean=?'([O1, O2]);
'equal?'(O1, O2) when is_atom(O1) ->
    scml_base_symbol:'symbol=?'([O1, O2]);
'equal?'({SHA, A}=O1, O2) when is_atom(SHA), is_binary(A) ->
    scml_base_symbol:'symbol=?'([O1, O2]);
'equal?'(O1, O2) when is_number(O1) ->
    'number=?'(O1, O2);
'equal?'(O1, O2) when tuple_size(O1)==2 ->
    'number=?'(O1, O2);
'equal?'(#character{}=O1, #character{}=O2) ->
    scml_base_char:'char=?'([O1, O2]);
'equal?'(#vector{}=O1, #vector{}=O2) ->
    'equal?'(scml_base_vector:'vector->list'(O1), scml_base_vector:'vector->list'(O2));
'equal?'(#bytevector{val=A}, #bytevector{val=A}) ->
    ?TRUE;
'equal?'(#string{}=O1, #string{}=O2) ->
    scml_base_string:'string=?'([O1, O2]);
'equal?'(#label{val={C,A}}, #label{val={C,B}}) ->
    'equal?'(A, B);
'equal?'(#labelref{val=A}, #labelref{val=A}) ->
    ?TRUE;
'equal?'(O1, O1) when is_pid(O1) ->
    ?TRUE;
'equal?'([], []) ->
    ?TRUE;
'equal?'([AH|AT], [BH|BT]) ->
    case 'equal?'(AH, BH) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'equal?'(AT, BT)
    end;
'equal?'(#nip0{val=A}, #nip0{val=A}) ->
    ?TRUE;
'equal?'(#nipn{val=A}, #nipn{val=A}) ->
    ?TRUE;
'equal?'(#nipv{val=A}, #nipv{val=A}) ->
    ?TRUE;
'equal?'(#nipnv{val=A}, #nipnv{val=A}) ->
    ?TRUE;
'equal?'(#xnip0{val=A}, #xnip0{val=A}) ->
    ?TRUE;
'equal?'(#xnipn{val=A}, #xnipn{val=A}) ->
    ?TRUE;
'equal?'(#xnipv{val=A}, #xnipv{val=A}) ->
    ?TRUE;
'equal?'(#xnipnv{val=A}, #xnipnv{val=A}) ->
    ?TRUE;
'equal?'(#lip0{val=A}, #lip0{val=A}) ->
    ?TRUE;
'equal?'(#lip0{val=#l0{src=AS}}, #lip0{val=#l0{src=BS}}) ->
    'equal-lambda?'(AS, BS);
'equal?'(#lipn{val=A}, #lipn{val=A}) ->
    ?TRUE;
'equal?'(#lipn{val=#ln{params=APs, src=AS}}, #lipn{val=#ln{params=BPs, src=BS}}) ->
    'equal-lambda?'(APs, AS, BPs, BS);
'equal?'(#lipv{val=A}, #lipv{val=A}) ->
    ?TRUE;
'equal?'(#lipv{val=#lv{param=APs, src=AS}}, #lipv{val=#lv{param=BPs, src=BS}}) ->
    'equal-lambda?'(APs, AS, BPs, BS);
'equal?'(#lipnv{val=A}, #lipnv{val=A}) ->
    ?TRUE;
'equal?'(#lipnv{val=#lnv{n=N, params=APs, src=AS}}, #lipnv{val=#lnv{n=N, params=BPs, src=BS}}) ->
    'equal-lambda?'(APs, AS, BPs, BS);
'equal?'(_, _) ->
    ?FALSE.

%%%===================================================================
%%% internal helpers
%%%===================================================================

'number=?'(O1, O2) ->
    case {scml_base_number:'exact?'(O1), scml_base_number:'exact?'(O2)} of
        {A, A} ->
            scml_base_number:'='([O1, O2]);
        _ ->
            ?FALSE
    end.

'equal-lambda?'(AS, BS) ->
    'equal?'(AS(), BS()).

'equal-lambda?'(APs, AS, BPs, BS) ->
    case 'equal?'(APs, BPs) of
        ?FALSE ->
            ?FALSE;
        _ ->
            'equal-lambda?'(AS, BS)
    end.
