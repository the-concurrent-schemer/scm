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

%%% @doc Scheme interpreter syntactic analyzer for derived expressions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze_derived).

%% SCMI Exports
-export(['$scmi_exports'/0]).

%% External exports
-export([analyze_cond/2
         , analyze_case/2
         , analyze_and/2
         , analyze_or/2
         , analyze_when/2
         , analyze_unless/2
         , analyze_cond_expand/2
         , analyze_let/2
         , analyze_lets/2
         , analyze_letrec/2
         , analyze_letrecs/2
         , analyze_let_values/2
         , analyze_lets_values/2
         , analyze_letrec_values/2
         , analyze_begin/2
         , analyze_do/2
         , analyze_make_parameter/2
         , analyze_parameterize/2
         , analyze_guard/2
         , analyze_quasiquote/2
         , analyze_unquote/2
         , analyze_unquote_splicing/2
         , scan_out_internal_definitions/2
        ]).

-import(scmi_analyze, [analyze/2, validate_variables/1, flatten_variables/1, make_tmp_variables/1]).
-import(scmi_analyze_primitive, [analyze_sequence/2]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% SCMI Exports
%%%----------------------------------------------------------------------

-spec '$scmi_exports'() -> [{scm_symbol(), scmi_expander()}].
'$scmi_exports'() ->
    [{'cond', #expander{val=fun ?MODULE:'analyze_cond'/2}}
     , {'case', #expander{val=fun ?MODULE:'analyze_case'/2}}
     , {'and', #expander{val=fun ?MODULE:'analyze_and'/2}}
     , {'or', #expander{val=fun ?MODULE:'analyze_or'/2}}
     , {'when', #expander{val=fun ?MODULE:'analyze_when'/2}}
     , {'unless', #expander{val=fun ?MODULE:'analyze_unless'/2}}
     , {'cond-expand', #expander{val=fun ?MODULE:'analyze_cond_expand'/2}}
     , {'let', #expander{val=fun ?MODULE:'analyze_let'/2}}
     , {'let*', #expander{val=fun ?MODULE:'analyze_lets'/2}}
     , {'letrec', #expander{val=fun ?MODULE:'analyze_letrec'/2}}
     , {'letrec*', #expander{val=fun ?MODULE:'analyze_letrecs'/2}}
     , {'let-values', #expander{val=fun ?MODULE:'analyze_let_values'/2}}
     , {'let*-values', #expander{val=fun ?MODULE:'analyze_lets_values'/2}}
     , {'letrec-values', #expander{val=fun ?MODULE:'analyze_letrec_values'/2}}
     , {'begin', #expander{val=fun ?MODULE:'analyze_begin'/2}}
     , {'do', #expander{val=fun ?MODULE:'analyze_do'/2}}
     , {'make-parameter', #expander{val=fun ?MODULE:'analyze_make_parameter'/2}}
     , {'parameterize', #expander{val=fun ?MODULE:'analyze_parameterize'/2}}
     , {'guard', #expander{val=fun ?MODULE:'analyze_guard'/2}}
     , {'quasiquote', #expander{val=fun ?MODULE:'analyze_quasiquote'/2}}
     , {'unquote', #expander{val=fun ?MODULE:'analyze_unquote'/2}}
     , {'unquote-splicing', #expander{val=fun ?MODULE:'analyze_unquote_splicing'/2}}
    ].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_cond(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_cond(Exp, SEnv) ->
    analyze(expand_cond(Exp), SEnv).

-spec analyze_case(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_case(Exp, SEnv) ->
    analyze(expand_case(Exp), SEnv).

-spec analyze_and(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_and(Exp, SEnv) ->
    analyze(expand_and(Exp), SEnv).

-spec analyze_or(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_or(Exp, SEnv) ->
    analyze(expand_or(Exp), SEnv).

-spec analyze_when(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_when(Exp, SEnv) ->
    analyze(expand_when(Exp), SEnv).

-spec analyze_unless(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_unless(Exp, SEnv) ->
    analyze(expand_unless(Exp), SEnv).

-spec analyze_cond_expand(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_cond_expand(Exp, SEnv) ->
    analyze(expand_cond_expand(Exp), SEnv).

-spec analyze_let(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_let(Exp, SEnv) ->
    analyze(expand_let(Exp, SEnv), SEnv).

-spec analyze_lets(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_lets(Exp, SEnv) ->
    analyze(expand_lets(Exp, SEnv), SEnv).

-spec analyze_letrec(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_letrec(Exp, SEnv) ->
    analyze(expand_letrec(Exp, SEnv), SEnv).

-spec analyze_letrecs(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_letrecs(Exp, SEnv) ->
    analyze(expand_letrecs(Exp, SEnv), SEnv).

-spec analyze_let_values(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_let_values(Exp, SEnv) ->
    analyze(expand_let_values(Exp, SEnv), SEnv).

-spec analyze_lets_values(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_lets_values(Exp, SEnv) ->
    analyze(expand_lets_values(Exp, SEnv), SEnv).

-spec analyze_letrec_values(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_letrec_values(Exp, SEnv) ->
    analyze(expand_letrec_values(Exp, SEnv), SEnv).

-spec analyze_begin(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_begin(Exp, SEnv) ->
    analyze_sequence(Exp, SEnv).

-spec analyze_do(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_do(Exp, SEnv) ->
    analyze(expand_do(Exp), SEnv).

-spec analyze_make_parameter(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_make_parameter(Exp, SEnv) ->
    analyze(expand_make_parameter(Exp), SEnv).

-spec analyze_parameterize(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_parameterize(Exp, SEnv) ->
    analyze(expand_parameterize(Exp, SEnv), SEnv).

-spec analyze_guard(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_guard(Exp, SEnv) ->
    analyze(expand_guard(Exp, SEnv), SEnv).

-spec analyze_quasiquote(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_quasiquote(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec analyze_unquote(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_unquote(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec analyze_unquote_splicing(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_unquote_splicing(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec scan_out_internal_definitions([scmi_exp(),...], scmi_senv()) -> [scmi_exp()].
scan_out_internal_definitions(Body, #senv{env=Env}) ->
    FunDV = fun scmi_analyze_program:'analyze_define_values'/2,
    FunD = fun scmi_analyze_program:'analyze_define'/2,

    HaveFunDV = case scmi_env:safe_lookup_variable('define-values', Env) of
                    #expander{val=FunDV} ->
                        true;
                    _ ->
                        false
                end,
    HaveFunD = case scmi_env:safe_lookup_variable('define', Env) of
                   #expander{val=FunD} ->
                       true;
                   _ ->
                       false
               end,

    Fun = fun(['define-values'|_], _) when HaveFunDV ->
                  define_values;
             (['define'|_], none) when HaveFunD ->
                  define;
             (_, Acc) ->
                  Acc
          end,
    case lists:foldl(Fun, none, Body) of
        define_values ->
            scan_out_internal_define_values(Body, HaveFunD);
        define ->
            scan_out_internal_define(Body);
        none ->
            Body
    end.

%%%----------------------------------------------------------------------
%%% Internal functions - derived conditionals
%%%----------------------------------------------------------------------

%% cond
expand_cond([]) ->
    ?FALSE;
expand_cond([['else'|Exp]]) ->
    make_begin(Exp);
expand_cond([[Test, '=>', Proc]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, make_call(Proc, [Tmp]), expand_cond(Exp))]);
expand_cond([[Test]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, expand_cond(Exp))]);
expand_cond([[Test|Exps]|Exp]) ->
    make_if(Test, make_begin(Exps), expand_cond(Exp)).

%% case
expand_case([Key|Exp]) ->
    Var = make_variable(),
    make_let([[Var, Key]], [from_case1(Var, Exp)]).

from_case1(_Var, []) ->
    ?FALSE;
from_case1(Var, [['else', '=>', Proc]]) ->
    make_call(Proc, [Var]);
from_case1(_Var, [['else'|Exp]]) ->
    make_begin(Exp);
from_case1(Var, [[Datums, '=>', Proc]|Exp]) ->
    Test = expand_or([ make_eqvp(Var, Datum) || Datum <- Datums ]),
    make_if(Test, make_call(Proc, [Var]), from_case1(Var, Exp));
from_case1(Var, [[Datums|Exps]|Exp]) ->
    Test = expand_or([ make_eqvp(Var, Datum) || Datum <- Datums ]),
    make_if(Test, make_begin(Exps), from_case1(Var, Exp)).

%% and
expand_and([]) ->
    ?TRUE;
expand_and([Test]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, ?FALSE)]);
expand_and([Test|Exp]) ->
    make_if(Test, expand_and(Exp), ?FALSE).

%% or
expand_or([]) ->
    ?FALSE;
expand_or([Test]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, ?FALSE)]);
expand_or([Test|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, expand_or(Exp))]).

%% when
expand_when([Test|Exp]) ->
    make_if(Test, make_begin(Exp ++ [?FALSE]), ?FALSE).

%% unless
expand_unless([Test|Exp]) ->
    make_if(Test, ?FALSE, make_begin(Exp ++ [?FALSE])).

%% cond-expand
expand_cond_expand([]) ->
    ?FALSE;
expand_cond_expand([['else'|Exp]]) ->
    make_begin(Exp);
expand_cond_expand([[Test|Exps]|Exp]) ->
    make_if(feature_test(Test), make_begin(Exps), expand_cond_expand(Exp)).

feature_test(['not', Test]) ->
    make_if(feature_test(Test), ?FALSE, ?TRUE);
feature_test(['or'|Tests]) ->
    make_or([ feature_test(Test) || Test <- Tests ]);
feature_test(['and'|Tests]) ->
    make_and([ feature_test(Test) || Test <- Tests ]);
feature_test(['library'|Test]) ->
    feature_test_library(Test);
feature_test(Identifier) ->
    feature_test_identifer(Identifier).

feature_test_library(Id) ->
    case lists:member(Id, scml:libraries()) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

feature_test_identifer(Id) ->
    case lists:member(Id, scml_base_system:features()) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%%%----------------------------------------------------------------------
%%% Internal functions - let derived binding constructs
%%%----------------------------------------------------------------------

%% let and named let
expand_let([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_let([Tag|[[]|Body]], SEnv) when not is_list(Tag) ->
    make_body_let_named(Tag, scan_out_internal_definitions(Body, SEnv));
expand_let([Tag|[Bindings|Body]], SEnv) when not is_list(Tag) ->
    from_let_named(Tag, Bindings, scan_out_internal_definitions(Body, SEnv));
expand_let([Bindings|Body], SEnv) ->
    from_let(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let*
expand_lets([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_lets([Bindings|Body], SEnv) ->
    from_lets(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% letrec
expand_letrec([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_letrec([Bindings|Body], SEnv) ->
    from_letrec(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% letrec*
expand_letrecs([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_letrecs([Bindings|Body], SEnv) ->
    from_letrecs(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let
from_let(Bindings, Body) ->
    from_let(Bindings, Body, [], []).

from_let([[Variable, Init]], Body, Variables, Inits) ->
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    make_body_let(Vs, Is, Body);
from_let([[Variable, Init]|Bindings], Body, Variables, Inits) ->
    from_let(Bindings, Body, [Variable|Variables], [Init|Inits]).

%% named let
from_let_named(Tag, Bindings, Body) ->
    from_let_named(Tag, Bindings, Body, [], []).

from_let_named(Tag, [[Variable, Init]], Body, Variables, Inits) ->
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    make_body_let_named(Tag, Vs, Is, Body);
from_let_named(Tag, [[Variable, Init]|Bindings], Body, Variables, Inits) ->
    from_let_named(Tag, Bindings, Body, [Variable|Variables], [Init|Inits]).

%% let*
from_lets([_Binding]=Bindings, Body) ->
    from_let(Bindings, Body);
from_lets([Binding|Bindings], Body) ->
    from_let([Binding], [from_lets(Bindings, Body)]).

%% letrec and letrec*
from_letrec(Bindings, Body) ->
    from_letrec(fun make_let/2, Bindings, Body, [], [], []).

from_letrecs(Bindings, Body) ->
    from_letrec(fun make_lets/2, Bindings, Body, [], [], []).

from_letrec(Make, [[Variable, Init]], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    Ts = lists:reverse([Tmp|Tmps]),
    make_body_letrec(Make, Vs, Is, Ts, Body);
from_letrec(Make, [[Variable, Init]|Bindings], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    from_letrec(Make, Bindings, Body, [Variable|Variables], [Init|Inits], [Tmp|Tmps]).

%% let, named let, letrec, and letrec* helpers
make_body_let(Variables, Args, Body) ->
    make_call(make_lambda(Variables, Body), Args).

make_body_let_named(Tag, Body) ->
    make_body_let_named(Tag, [], [], Body).

make_body_let_named(Tag, Variables, Args, Body) ->
    make_call(make_letrec([[Tag, make_lambda(Variables, Body)]], [Tag]), Args).

make_body_letrec(Make, Variables, Args, Tmps, Body) ->
    make_let([ [V, ?UNASSIGNED] || V <- Variables ],
             [Make([ [T, A] || {T, A} <- lists:zip(Tmps, Args) ],
                   [ make_setb(V, T) || {V, T} <- lists:zip(Variables, Tmps) ] ++ Body)]).

%%%----------------------------------------------------------------------
%%% Internal functions - let-values derived binding constructs
%%%----------------------------------------------------------------------

%% let-values
expand_let_values([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_let_values([Bindings|Body], SEnv) ->
    from_let_values(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let*-values
expand_lets_values([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_lets_values([Bindings|Body], SEnv) ->
    from_lets_values(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% letrec-values
expand_letrec_values([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_letrec_values([Bindings|Body], SEnv) ->
    from_letrec_values(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let-values
from_let_values(Bindings, Body) ->
    from_let_values(Bindings, Body, [], []).

from_let_values([[Formal, Init]], Body, Formals, Inits) ->
    Fs = lists:reverse([Formal|Formals]),
    Is = lists:reverse([Init|Inits]),
    make_body_let_values(Fs, Is, Body);
from_let_values([[Formal, Init]|Bindings], Body, Formals, Inits) ->
    from_let_values(Bindings, Body, [Formal|Formals], [Init|Inits]).

%% let*-values
from_lets_values([_Binding]=Bindings, Body) ->
    from_let_values(Bindings, Body);
from_lets_values([Binding|Bindings], Body) ->
    from_let_values([Binding], [from_lets_values(Bindings, Body)]).

%% letrec-values
from_letrec_values(Bindings, Body) ->
    from_letrec_values(Bindings, Body, [], [], []).

from_letrec_values([[Formal, Init]], Body, Formals, Inits, Tmps) ->
    Tmp = make_tmp_variables(Formal),
    Fs = lists:reverse([Formal|Formals]),
    Is = lists:reverse([Init|Inits]),
    Ts = lists:reverse([Tmp|Tmps]),
    make_body_letrec_values(Fs, Is, Ts, Body);
from_letrec_values([[Formal, Init]|Bindings], Body, Formals, Inits, Tmps) ->
    Tmp = make_tmp_variables(Formal),
    from_letrec_values(Bindings, Body, [Formal|Formals], [Init|Inits], [Tmp|Tmps]).

%% let-values and letrec-values helpers
make_body_let_values(Formals, Inits, Body) ->
    Fs = flatten_variables(Formals),
    validate_variables(Fs), % validate formals
    make_body_let_values1(Formals, Inits, Body).

make_body_letrec_values(Formals, Inits, Tmps, Body) ->
    Fs = flatten_variables(Formals),
    validate_variables(Fs), % validate formals
    Ts = flatten_variables(Tmps),
    LetRecBody = [ make_setb(F, T) || {F, T} <- lists:zip(Fs, Ts) ] ++ Body,
    make_let([ [F, ?UNASSIGNED] || F <- Fs ], [make_body_let_values1(Tmps, Inits, LetRecBody)]).

make_body_let_values1([F], [I], Body) ->
    make_call_with_values(make_thunk([I]), make_lambda(F, Body));
make_body_let_values1([F|Formals], [I|Inits], Body) ->
    make_call_with_values(make_thunk([I]), make_lambda(F, [make_body_let_values1(Formals, Inits, Body)])).

%%%----------------------------------------------------------------------
%%% Internal functions - iteration
%%%----------------------------------------------------------------------

%% do
expand_do([Specs, [Test]|Commands]) ->
    from_do(Specs, Test, [?FALSE], Commands);
expand_do([Specs, [Test|Exps]|Commands]) ->
    from_do(Specs, Test, Exps, Commands).

from_do(Specs, Test, Exps, Commands) ->
    from_do(Specs, Test, Exps, Commands, [], [], []).

from_do([], Test, Exps, Commands, Vars, Inits, Steps) ->
    Vs = lists:reverse(Vars),
    Is = lists:reverse(Inits),
    Ss = lists:reverse(Steps),
    from_do1(Vs, Is, Ss, Test, Exps, Commands);
from_do([[Var, Init]|Specs], Test, Exps, Commands, Vars, Inits, Steps) ->
    from_do(Specs, Test, Exps, Commands, [Var|Vars], [Init|Inits], [Var|Steps]);
from_do([[Var, Init, Step]|Specs], Test, Exps, Commands, Vars, Inits, Steps) ->
    from_do(Specs, Test, Exps, Commands, [Var|Vars], [Init|Inits], [Step|Steps]).

from_do1(Vars, Inits, Steps, Test, Exps, Commands) ->
    Loop = make_variable(),
    Repeat = make_call(Loop, Steps),
    Body = make_if(Test, make_begin(Exps), make_begin(Commands ++ [Repeat])),
    make_let_named(Loop, [ [V, I] || {V, I} <- lists:zip(Vars, Inits) ], [Body]).

%%%----------------------------------------------------------------------
%%% Internal functions - dynamic bindings
%%%----------------------------------------------------------------------

%% make-parameter
expand_make_parameter([Init]) ->
    X = make_variable(),
    from_make_parameter(Init, make_lambda([X], [X]));
expand_make_parameter([Init|Converter]) ->
    from_make_parameter(Init, Converter).

from_make_parameter(Init, Converter) ->
    Cvt = make_variable(),
    Val = make_variable(),
    Args = make_variable(),
    make_lets([[Cvt, Converter], [Val, make_call(Converter, [Init])]],
              [make_lambda(Args,
                           [make_cond([[make_nullp(Args), Val],
                                       [make_eqp(make_car(Args), ?SCMIPARAMSET), make_setb(Val, make_cadr(Args))],
                                       [make_eqp(make_car(Args), ?SCMIPARAMCVT), Cvt],
                                       make_else([make_error(#string{val= <<"bad parameter syntax">>})])])])]).

%% parameterize
expand_parameterize([[]|Body], SEnv) ->
    make_begin(scan_out_internal_definitions(Body, SEnv));
expand_parameterize([Parameters|Body], SEnv) ->
    from_parameterize(Parameters, scan_out_internal_definitions(Body, SEnv)).

from_parameterize(Parameters, Body) ->
    from_parameterize(Parameters, Body, [], [], [], [], []).

from_parameterize([], Body, Params, Vals, Ps, Olds, News) ->
    make_let([ [P, Param] || {P, Param} <- lists:zip(Ps, Params) ],
             [make_let([ [Old, make_call(P)] || {P, Old} <- lists:zip(Ps, Olds) ]
                       ++ [ [New, make_call(make_call(P, [?SCMIPARAMCVT]), [Val]) ] || {P, New, Val} <- lists:zip3(Ps, News, Vals) ],
                       [make_dynamic_wind(
                          make_thunk([ make_call(P, [?SCMIPARAMSET, New]) || {P, New} <- lists:zip(Ps, News) ]),
                          make_thunk(Body),
                          make_thunk([ make_call(P, [?SCMIPARAMSET, Old]) || {P, Old} <- lists:zip(Ps, Olds) ]))])]);
from_parameterize([[Param, Val]|Parameters], Body, Params, Vals, Ps, Olds, News) ->
    P = make_variable(),
    Old = make_variable(),
    New = make_variable(),
    from_parameterize(Parameters, Body, [Param|Params], [Val|Vals], [P|Ps], [Old|Olds], [New|News]).

%%%----------------------------------------------------------------------
%%% Internal functions - exception handling
%%%----------------------------------------------------------------------

%% guard
expand_guard([[Var|Cond]|Body], SEnv) ->
    from_guard(Var, Cond, scan_out_internal_definitions(Body, SEnv)).

from_guard(Var, Cond, Body) ->
    GuardK = make_variable(),
    HandlerK = make_variable(),
    Condition = make_variable(),
    Args = make_variable(),

    Reraise = make_call(HandlerK, [make_thunk([make_raise_continuable(Condition)])]),
    HandlerCall = make_call(GuardK, [make_thunk([make_let([[Var, Condition]], [from_guard_cond(Reraise, Cond)])])]),
    Handler = make_lambda([Condition], [make_call(make_callcc(make_lambda([HandlerK], [HandlerCall])))]),

    ThunkCall = make_call(GuardK, [make_thunk([make_apply('values', [Args])])]),
    Thunk = make_thunk([make_call_with_values(make_thunk(Body), make_lambda(Args, [ThunkCall]))]),

    make_call(make_callcc(make_lambda([GuardK], [make_with_exception_handler(Handler, Thunk)]))).

from_guard_cond(Reraise, []) ->
    Reraise;
from_guard_cond(_Reraise, [['else'|Exp]]) ->
    make_begin(Exp);
from_guard_cond(Reraise, [[Test, '=>', Proc]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, make_call(Proc, [Tmp]), from_guard_cond(Reraise, Exp))]);
from_guard_cond(Reraise, [[Test]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, from_guard_cond(Reraise, Exp))]);
from_guard_cond(Reraise, [[Test|Exps]|Exp]) ->
    make_if(Test, make_begin(Exps), from_guard_cond(Reraise, Exp)).

%%%----------------------------------------------------------------------
%%% Internal functions - quasiquotation
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Internal functions - scan_out_internal_definitions
%%%----------------------------------------------------------------------

scan_out_internal_define_values(Body, HaveD) ->
    scan_out_internal_define_values(Body, HaveD, [], []).

scan_out_internal_define_values([], _HaveD, Defines, Body) ->
    [make_letrec_values(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_define_values([['define-values'|H]|T], HaveD, Defines, Body) ->
    scan_out_internal_define_values(T, HaveD, [define_values_to_values_binding(H)|Defines], Body);
scan_out_internal_define_values([['define'|H]|T], true, Defines, Body) ->
    scan_out_internal_define_values(T, true, [define_to_values_binding(H)|Defines], Body);
scan_out_internal_define_values([H|T], HaveD, Defines, Body) ->
    scan_out_internal_define_values(T, HaveD, Defines, [H|Body]).

scan_out_internal_define(Body) ->
    scan_out_internal_define(Body, [], []).

scan_out_internal_define([], Defines, Body) ->
    [make_letrecs(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_define([['define'|H]|T], Defines, Body) ->
    scan_out_internal_define(T, [define_to_binding(H)|Defines], Body);
scan_out_internal_define([H|T], Defines, Body) ->
    scan_out_internal_define(T, Defines, [H|Body]).

define_values_to_values_binding([Formal, Init]) ->
    [Formal, Init];
define_values_to_values_binding([Formal|Init]) ->
    [Formal, Init].

define_to_values_binding(Exp) ->
    case define_to_binding(Exp) of
        [Variable, Exp1] ->
            [[Variable], make_values([Exp1])]
    end.

%% (define (<variable> <identifier>*) <body>)
define_to_binding([[Variable|Formals]|Body]) when not is_list(Variable) ->
    [Variable, make_lambda(Formals, Body)];
%% (define (<variable> . <identifier>) <body>)
define_to_binding([[[Variable]|Formal]|Body]) when not is_list(Formal) ->
    [Variable, make_lambda(Formal, Body)];
%% (define (<variable> <identifier>+ . <identifier>) <body>)
define_to_binding([[[Variable|Formals]|Formal]|Body]) when not is_list(Formal) ->
    [Variable, make_lambda([Formals|Formal], Body)];
%% (define <variable> <expression>)
define_to_binding([Variable, Exp]) when not is_list(Variable) ->
    [Variable, Exp].
