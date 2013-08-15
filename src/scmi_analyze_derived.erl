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
         , scan_out_internal_definitions/1
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

-spec '$scmi_exports'() -> [{scm_symbol(), scmi_sugar()}].
'$scmi_exports'() ->
    [{'cond', #sugar{val=fun 'analyze_cond'/2}}
     , {'case', #sugar{val=fun 'analyze_case'/2}}
     , {'and', #sugar{val=fun 'analyze_and'/2}}
     , {'or', #sugar{val=fun 'analyze_or'/2}}
     , {'when', #sugar{val=fun 'analyze_when'/2}}
     , {'unless', #sugar{val=fun 'analyze_unless'/2}}
     , {'cond-expand', #sugar{val=fun 'analyze_cond_expand'/2}}
     , {'let', #sugar{val=fun 'analyze_let'/2}}
     , {'let*', #sugar{val=fun 'analyze_lets'/2}}
     , {'letrec', #sugar{val=fun 'analyze_letrec'/2}}
     , {'letrec*', #sugar{val=fun 'analyze_letrecs'/2}}
     , {'let-values', #sugar{val=fun 'analyze_let_values'/2}}
     , {'let*-values', #sugar{val=fun 'analyze_lets_values'/2}}
     , {'letrec-values', #sugar{val=fun 'analyze_letrec_values'/2}}
     , {'begin', #sugar{val=fun 'analyze_begin'/2}}
     , {'do', #sugar{val=fun 'analyze_do'/2}}
     , {'make-parameter', #sugar{val=fun 'analyze_make_parameter'/2}}
     , {'parameterize', #sugar{val=fun 'analyze_parameterize'/2}}
     , {'guard', #sugar{val=fun 'analyze_guard'/2}}
     , {'quasiquote', #sugar{val=fun 'analyze_quasiquote'/2}}
     , {'unquote', #sugar{val=fun 'analyze_unquote'/2}}
     , {'unquote-splicing', #sugar{val=fun 'analyze_unquote_splicing'/2}}
    ].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_cond(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_cond(Exp, SEnv) ->
    analyze(from_cond(Exp), SEnv).

-spec analyze_case(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_case(Exp, SEnv) ->
    analyze(from_case(Exp), SEnv).

-spec analyze_and(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_and(Exp, SEnv) ->
    analyze(from_and(Exp), SEnv).

-spec analyze_or(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_or(Exp, SEnv) ->
    analyze(from_or(Exp), SEnv).

-spec analyze_when(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_when(Exp, SEnv) ->
    analyze(from_when(Exp), SEnv).

-spec analyze_unless(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_unless(Exp, SEnv) ->
    analyze(from_unless(Exp), SEnv).

-spec analyze_cond_expand(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_cond_expand(Exp, SEnv) ->
    analyze(from_cond_expand(Exp), SEnv).

-spec analyze_let(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_let(Exp, SEnv) ->
    analyze(from_let(Exp), SEnv).

-spec analyze_lets(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_lets(Exp, SEnv) ->
    analyze(from_lets(Exp), SEnv).

-spec analyze_letrec(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_letrec(Exp, SEnv) ->
    analyze(from_letrec(Exp), SEnv).

-spec analyze_letrecs(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_letrecs(Exp, SEnv) ->
    analyze(from_letrecs(Exp), SEnv).

-spec analyze_let_values(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_let_values(Exp, SEnv) ->
    analyze(from_let_values(Exp), SEnv).

-spec analyze_lets_values(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_lets_values(Exp, SEnv) ->
    analyze(from_lets_values(Exp), SEnv).

-spec analyze_letrec_values(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_letrec_values(Exp, SEnv) ->
    analyze(from_letrec_values(Exp), SEnv).

-spec analyze_begin(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_begin(Exp, SEnv) ->
    analyze_sequence(Exp, SEnv).

-spec analyze_do(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_do(Exp, SEnv) ->
    analyze(from_do(Exp), SEnv).

-spec analyze_make_parameter(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_make_parameter(Exp, SEnv) ->
    analyze(from_make_parameter(Exp), SEnv).

-spec analyze_parameterize(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_parameterize(Exp, SEnv) ->
    analyze(from_parameterize(Exp), SEnv).

-spec analyze_guard(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_guard(Exp, SEnv) ->
    analyze(from_guard(Exp), SEnv).

-spec analyze_quasiquote(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_quasiquote(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec analyze_unquote(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_unquote(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec analyze_unquote_splicing(scm_any(), scmi_senv()) -> scmi_dexec().
analyze_unquote_splicing(Exp, SEnv) ->
    %% @TODO
    erlang:error({roadmap,'v0.5.0'}, [Exp, SEnv]).

-spec scan_out_internal_definitions([scm_any(),...]) -> [scm_any()].
scan_out_internal_definitions(Body) ->
    Fun = fun(['define-values'|_], _) ->
                  define_values;
             (['define'|_], none) ->
                  defines;
             (_, Acc) ->
                  Acc
          end,
    case lists:foldl(Fun, none, Body) of
        define_values ->
            scan_out_internal_define_values(Body);
        defines ->
            scan_out_internal_defines(Body);
        none ->
            Body
    end.

%%%----------------------------------------------------------------------
%%% Internal functions - derived conditionals
%%%----------------------------------------------------------------------

%% cond
from_cond([]) ->
    ?FALSE;
from_cond([['else'|Exp]]) ->
    make_begin(Exp);
from_cond([[Test, '=>', Proc]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, make_call(Proc, [Tmp]), from_cond(Exp))]);
from_cond([[Test]|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, from_cond(Exp))]);
from_cond([[Test|Exps]|Exp]) ->
    make_if(Test, make_begin(Exps), from_cond(Exp)).

%% case
from_case([Key|Exp]) ->
    Var = make_variable(),
    make_let([[Var, Key]], [from_case1(Var, Exp)]).

from_case1(_Var, []) ->
    ?FALSE;
from_case1(Var, [['else', '=>', Proc]]) ->
    make_call(Proc, [Var]);
from_case1(_Var, [['else'|Exp]]) ->
    make_begin(Exp);
from_case1(Var, [[Datums, '=>', Proc]|Exp]) ->
    Test = from_or([ make_eqvp(Var, Datum) || Datum <- Datums ]),
    make_if(Test, make_call(Proc, [Var]), from_case1(Var, Exp));
from_case1(Var, [[Datums|Exps]|Exp]) ->
    Test = from_or([ make_eqvp(Var, Datum) || Datum <- Datums ]),
    make_if(Test, make_begin(Exps), from_case1(Var, Exp)).

%% and
from_and([]) ->
    ?TRUE;
from_and([Test]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, ?FALSE)]);
from_and([Test|Exp]) ->
    make_if(Test, from_and(Exp), ?FALSE).

%% or
from_or([]) ->
    ?FALSE;
from_or([Test]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, ?FALSE)]);
from_or([Test|Exp]) ->
    Tmp = make_variable(),
    make_let([[Tmp, Test]], [make_if(Tmp, Tmp, from_or(Exp))]).

%% when
from_when([Test|Exp]) ->
    make_if(Test, make_begin(Exp ++ [?FALSE]), ?FALSE).

%% unless
from_unless([Test|Exp]) ->
    make_if(Test, ?FALSE, make_begin(Exp ++ [?FALSE])).

%% cond-expand
from_cond_expand([]) ->
    ?FALSE;
from_cond_expand([['else'|Exp]]) ->
    make_begin(Exp);
from_cond_expand([[Test|Exps]|Exp]) ->
    make_if(feature_test(Test), make_begin(Exps), from_cond_expand(Exp)).

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
from_let([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_let([Tag|[[]|Body]]) when not is_list(Tag) ->
    make_body_let_named(Tag, scan_out_internal_definitions(Body));
from_let([Tag|[Bindings|Body]]) when not is_list(Tag) ->
    from_let_named(Tag, Bindings, scan_out_internal_definitions(Body));
from_let([Bindings|Body]) ->
    from_let(Bindings, scan_out_internal_definitions(Body)).

%% let*
from_lets([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_lets([Bindings|Body]) ->
    from_lets(Bindings, scan_out_internal_definitions(Body)).

%% letrec
from_letrec([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_letrec([Bindings|Body]) ->
    from_letrec(Bindings, scan_out_internal_definitions(Body)).

%% letrec*
from_letrecs([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_letrecs([Bindings|Body]) ->
    from_letrecs(Bindings, scan_out_internal_definitions(Body)).

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
from_let_values([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_let_values([Bindings|Body]) ->
    from_let_values(Bindings, scan_out_internal_definitions(Body)).

%% let*-values
from_lets_values([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_lets_values([Bindings|Body]) ->
    from_lets_values(Bindings, scan_out_internal_definitions(Body)).

%% letrec-values
from_letrec_values([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_letrec_values([Bindings|Body]) ->
    from_letrec_values(Bindings, scan_out_internal_definitions(Body)).

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
from_do([Specs, [Test]|Commands]) ->
    from_do(Specs, Test, [?FALSE], Commands);
from_do([Specs, [Test|Exps]|Commands]) ->
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
from_make_parameter([Init]) ->
    X = make_variable(),
    from_make_parameter(Init, make_lambda([X], [X]));
from_make_parameter([Init|Converter]) ->
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
from_parameterize([[]|Body]) ->
    make_begin(scan_out_internal_definitions(Body));
from_parameterize([Parameters|Body]) ->
    from_parameterize(Parameters, scan_out_internal_definitions(Body)).

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
from_guard([[Var|Cond]|Body]) ->
    from_guard(Var, Cond, scan_out_internal_definitions(Body)).

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

scan_out_internal_define_values(Body) ->
    scan_out_internal_define_values(Body, [], []).

scan_out_internal_define_values([], Defines, Body) ->
    [make_letrec_values(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_define_values([['define-values'|H]|T], Defines, Body) ->
    scan_out_internal_define_values(T, [define_values_to_values_binding(H)|Defines], Body);
scan_out_internal_define_values([['define'|H]|T], Defines, Body) ->
    scan_out_internal_define_values(T, [define_to_values_binding(H)|Defines], Body);
scan_out_internal_define_values([H|T], Defines, Body) ->
    scan_out_internal_define_values(T, Defines, [H|Body]).

scan_out_internal_defines(Body) ->
    scan_out_internal_defines(Body, [], []).

scan_out_internal_defines([], Defines, Body) ->
    [make_letrecs(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_defines([['define'|H]|T], Defines, Body) ->
    scan_out_internal_defines(T, [define_to_binding(H)|Defines], Body);
scan_out_internal_defines([H|T], Defines, Body) ->
    scan_out_internal_defines(T, Defines, [H|Body]).

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
