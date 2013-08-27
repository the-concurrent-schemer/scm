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

%%% @doc Scheme interpreter syntactic analyzer
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze).

%% External exports
-export([the_default/0
         , analyze/1
         , analyze/2
         , classify/1
         , is_reserved_symbol/1
         , are_valid_variables/1
         , is_valid_variable/1
         , validate_variables/1
         , validate_variable/1
         , flatten_variables/1
         , make_tmp_variables/1
         , splitnv_arguments/2
        ]).

%% External types
-export_type([senv/0
             ]).

%% Internal imports
-import(scmi_analyze_primitive, [analyze_application/2]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% analyze
-type senv() :: #senv{}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec the_default() -> scmi_senv().
the_default() ->
    Env = scmi_env:the_empty(),
    Fun = fun(M) ->
                  [ scmi_env:define_variable(K, V, Env) || {K, V} <- M:'$scmi_exports'() ]
          end,
    Ms = [scmi_analyze_primitive, scmi_analyze_derived, scmi_analyze_macro, scmi_analyze_program],
    lists:foreach(Fun, Ms),
    #senv{env=Env}.

-spec analyze(scmi_exp()) -> scmi_expander() | scmi_dexec().
analyze(Exp) ->
    analyze(Exp, the_default()).

-spec analyze(scmi_exp(), scmi_senv()) -> scmi_expander() | scmi_dexec().
analyze(Exp, SEnv) when is_number(Exp) ->
    analyze_self_evaluating(Exp, SEnv);
analyze({Num, Den}=Exp, SEnv) when is_number(Num), is_number(Den) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?PINF=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?NINF=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?PNAN=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?NNAN=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?NZER=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze({Complex, {_A, _B}}=Exp, SEnv) when Complex==rectangular; Complex==polar ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, boolean) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, bytevector) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, character) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, string) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, vector) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(?UNASSIGNED=Exp, SEnv) ->
    analyze_self_evaluating(Exp, SEnv);
analyze(Exp, SEnv) when is_atom(Exp) ->
    analyze_variable(Exp, SEnv);
analyze({Sha1, Var}=Exp, SEnv) when is_atom(Sha1), is_binary(Var) ->
    analyze_variable(Exp, SEnv);
analyze(Exp, SEnv) when is_reference(Exp) ->
    analyze_variable(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, label) ->
    analyze_label(Exp, SEnv);
analyze(Exp, SEnv) when is_record(Exp, labelref) ->
    analyze_labelref(Exp, SEnv);
analyze([_Rator|Rands]=Exp, SEnv) when is_list(Rands) ->
    analyze_expression(Exp, SEnv);
analyze(Exp, SEnv) ->
    erlang:error(badarg, [Exp, SEnv]).

-spec classify(scmi_exp()) -> atom() | {rectangular | polar, {atom(), atom()}}.
classify(Exp) when is_integer(Exp) ->
    integer;
classify(Exp) when is_float(Exp) ->
    float;
classify({Num, Den}) when is_number(Num), is_number(Den) ->
    rational;
classify(?PINF) ->
    pinf;
classify(?NINF) ->
    ninf;
classify(?PNAN) ->
    pnan;
classify(?NNAN) ->
    nnan;
classify(?NZER) ->
    nzer;
classify({Complex, {A, B}}) when Complex==rectangular; Complex==polar ->
    {Complex, {classify_real(A), classify_real(B)}};
classify(Exp) when is_record(Exp, boolean) ->
    boolean;
classify(Exp) when is_record(Exp, bytevector) ->
    bytevector;
classify(Exp) when is_record(Exp, character) ->
    character;
classify(Exp) when is_record(Exp, string) ->
    string;
classify(Exp) when is_record(Exp, vector) ->
    vector;
classify(?UNASSIGNED) ->
    unassigned;
classify(Exp) when is_atom(Exp) ->
    identifier;
classify({Sha1, Var}) when is_atom(Sha1), is_binary(Var) ->
    identifier;
classify(Exp) when is_reference(Exp) ->
    variable;
classify(Exp) when is_record(Exp, label) ->
    label;
classify(Exp) when is_record(Exp, labelref) ->
    labelref;
classify([_Rator|Rands]) when is_list(Rands) ->
    expression;
classify([]) ->
    nil;
classify(Exp) ->
    erlang:error(badarg, [Exp]).

-spec classify_real(scmi_exp()) -> atom().
classify_real(Exp) when is_integer(Exp) ->
    integer;
classify_real(Exp) when is_float(Exp) ->
    float;
classify_real({Num, Den}) when is_number(Num), is_number(Den) ->
    rational;
classify_real(?PINF) ->
    pinf;
classify_real(?NINF) ->
    ninf;
classify_real(?PNAN) ->
    pnan;
classify_real(?NNAN) ->
    nnan;
classify_real(?NZER) ->
    nzer;
classify_real(Exp) ->
    erlang:error(badarg, [Exp]).

-spec is_reserved_symbol(scmi_var()) -> boolean().
is_reserved_symbol(?UNASSIGNED) ->
    true;
is_reserved_symbol(_) ->
    false.

-spec are_valid_variables([scmi_var()]) -> boolean().
are_valid_variables([Variables|Variable]) when not is_list(Variable) ->
    are_valid_variables([Variable|Variables]);
are_valid_variables(Variables) when is_list(Variables) ->
    lists:all(fun is_valid_variable/1, Variables) andalso
        lists:sort(Variables) =:= lists:usort(Variables);
are_valid_variables(_) ->
    false.

-spec is_valid_variable(scmi_var()) -> boolean().
is_valid_variable(Variable) when not is_list(Variable) ->
    case classify(Variable) of
        identifier ->
            not is_reserved_symbol(Variable);
        variable ->
            true;
        _ ->
            false
    end;
is_valid_variable(_) ->
    false.

-spec validate_variables([scmi_var()]) -> true.
validate_variables(Variables) ->
    case are_valid_variables(Variables) of
        true ->
            true;
        false ->
            erlang:error(badarg, [Variables])
    end.

-spec validate_variable(scmi_var()) -> true.
validate_variable(Variable) ->
    case is_valid_variable(Variable) of
        true ->
            true;
        false ->
            erlang:error(badarg, [Variable])
    end.

-spec flatten_variables(scmi_var() | [scmi_var()]) -> [scmi_var()].
flatten_variables(L) ->
    lists:flatten(flatten_variables1(L)).

flatten_variables1(V) when not is_list(V) ->
    [V];
flatten_variables1([H|T]) when not is_list(T) ->
    flatten_variables1(H ++ [T]);
flatten_variables1(L) ->
    [ flatten_variables1(V) || V <- L ].

-spec make_tmp_variables(scmi_var() | [scmi_var()]) -> scmi_var() | [scmi_var()].
make_tmp_variables(Formal) when not is_list(Formal) ->
    make_variable();
make_tmp_variables([Formals|Formal]) when not is_list(Formal) ->
    [[ make_variable() || _ <- Formals ]|make_variable()];
make_tmp_variables(Formals) ->
    [ make_variable() || _ <- Formals ].

-spec splitnv_arguments(pos_integer(), [scmi_exp(),...]) -> [scmi_exp(),...].
splitnv_arguments(N, L) ->
    splitnv_arguments(N, L, []).

splitnv_arguments(0, L, R) ->
    lists:reverse(R, [L]);
splitnv_arguments(N, [H|T], R) ->
    splitnv_arguments(N-1, T, [H|R]);
splitnv_arguments(_, []=L, R) ->
    lists:reverse(R, [L]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec analyze_label(scmi_exp(), scmi_senv()) -> no_return().
analyze_label(Exp, SEnv) ->
    erlang:error(unsupported, [Exp, SEnv]).

-spec analyze_labelref(scmi_exp(), scmi_senv()) -> no_return().
analyze_labelref(Exp, SEnv) ->
    erlang:error(unsupported, [Exp, SEnv]).

-spec analyze_self_evaluating(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_self_evaluating(Exp, _SEnv) ->
    fun(_Env, Ok, Ng) -> Ok(Exp, Ng) end.

-spec analyze_variable(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_variable(Exp, _SEnv) ->
    fun(Env, Ok, Ng) -> Ok(scmi_env:lookup_variable(Exp, Env), Ng) end.

-spec analyze_expression(scmi_exp(), scmi_senv()) -> scmi_expander() | scmi_dexec().
analyze_expression([Rator|Rands]=Exp, #senv{env=Env}=SEnv) ->
    case scmi_env:safe_lookup_variable(Rator, Env) of
        ?UNASSIGNED ->
            analyze_application(Exp, SEnv);
        #expander{val=Fun} ->
            Fun(Rands, SEnv);
        _ ->
            erlang:error(badarg, [Exp, SEnv])
    end.
