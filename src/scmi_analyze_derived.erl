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

-module(scmi_analyze_derived).

-include("scmi.hrl").
-include("scmi_analyze.hrl").

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
-import(scmi, [make_and/1
               , make_begin/1
               , make_call/2
               , make_case/2, make_case/4
               , make_if/3
               , make_lambda/2
               , make_let/2
               , make_let_named/3
               , make_letrec_values/2
               , make_letrec/2
               , make_letrecs/2
               , make_or/1
               , make_set/2
               , make_variable/0
              ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_cond(scm_any(), scmi_ana()) -> scmi_exec().
analyze_cond(Exp, Ana) ->
    analyze(from_cond(Exp), Ana).

-spec analyze_case(scm_any(), scmi_ana()) -> scmi_exec().
analyze_case(Exp, Ana) ->
    analyze(from_case(Exp), Ana).

-spec analyze_and(scm_any(), scmi_ana()) -> scmi_exec().
analyze_and(Exp, Ana) ->
    analyze(from_and(Exp), Ana).

-spec analyze_or(scm_any(), scmi_ana()) -> scmi_exec().
analyze_or(Exp, Ana) ->
    analyze(from_or(Exp), Ana).

-spec analyze_when(scm_any(), scmi_ana()) -> scmi_exec().
analyze_when(Exp, Ana) ->
    analyze(from_when(Exp), Ana).

-spec analyze_unless(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unless(Exp, Ana) ->
    analyze(from_unless(Exp), Ana).

-spec analyze_cond_expand(scm_any(), scmi_ana()) -> scmi_exec().
analyze_cond_expand(Exp, Ana) ->
    analyze(from_cond_expand(Exp), Ana).

-spec analyze_let(scm_any(), scmi_ana()) -> scmi_exec().
analyze_let(Exp, Ana) ->
    analyze(from_let(Exp), Ana).

-spec analyze_lets(scm_any(), scmi_ana()) -> scmi_exec().
analyze_lets(Exp, Ana) ->
    analyze(from_lets(Exp), Ana).

-spec analyze_letrec(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrec(Exp, Ana) ->
    analyze(from_letrec(Exp), Ana).

-spec analyze_letrecs(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrecs(Exp, Ana) ->
    analyze(from_letrecs(Exp), Ana).

-spec analyze_let_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_let_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_lets_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_lets_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_letrec_values(scm_any(), scmi_ana()) -> scmi_exec().
analyze_letrec_values(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_begin(scm_any(), scmi_ana()) -> scmi_exec().
analyze_begin(Exp, Ana) ->
    analyze_sequence(Exp, Ana).

-spec analyze_do(scm_any(), scmi_ana()) -> scmi_exec().
analyze_do(Exp, Ana) ->
    analyze(from_do(Exp), Ana).

-spec analyze_make_parameter(scm_any(), scmi_ana()) -> scmi_exec().
analyze_make_parameter(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_parameterize(scm_any(), scmi_ana()) -> scmi_exec().
analyze_parameterize(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_guard(scm_any(), scmi_ana()) -> scmi_exec().
analyze_guard(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.3.0'}, [Exp, Ana]).

-spec analyze_quasiquote(scm_any(), scmi_ana()) -> scmi_exec().
analyze_quasiquote(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

-spec analyze_unquote(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unquote(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

-spec analyze_unquote_splicing(scm_any(), scmi_ana()) -> scmi_exec().
analyze_unquote_splicing(Exp, Ana) ->
    %% @TODO
    erlang:error({roadmap,'v0.7.0'}, [Exp, Ana]).

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
            %% @TODO
            erlang:error({roadmap,'v0.3.0'}, [Body]);
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
    Test = from_or([ make_call('eqv?', [Var, Datum]) || Datum <- Datums ]),
    make_if(Test, make_call(Proc, [Var]), from_case1(Var, Exp));
from_case1(Var, [[Datums|Exps]|Exp]) ->
    Test = from_or([ make_call('eqv?', [Var, Datum]) || Datum <- Datums ]),
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
%%% Internal functions - derived binding constructs
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

%% letrec
from_letrec(Bindings, Body) ->
    from_letrec(Bindings, Body, [], [], []).

from_letrec([[Variable, Init]], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    Ts = lists:reverse([Tmp|Tmps]),
    make_body_letrec(Vs, Is, Ts, Body);
from_letrec([[Variable, Init]|Bindings], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    from_letrec(Bindings, Body, [Variable|Variables], [Init|Inits], [Tmp|Tmps]).

%% letrec*
from_letrecs(Bindings, Body) ->
    from_letrecs(Bindings, Body, [], [], []).

from_letrecs([[Variable, Init]], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    Ts = lists:reverse([Tmp|Tmps]),
    make_body_letrecs(Vs, Is, Ts, Body);
from_letrecs([[Variable, Init]|Bindings], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    from_letrecs(Bindings, Body, [Variable|Variables], [Init|Inits], [Tmp|Tmps]).

%% let, named let, letrec, and letrec* helpers
make_body_let(Variables, Args, Body) ->
    make_call(make_lambda(Variables, Body), Args).

make_body_let_named(Tag, Body) ->
    make_body_let_named(Tag, [], [], Body).

make_body_let_named(Tag, Variables, Args, Body) ->
    make_call(make_letrec([[Tag, make_lambda(Variables, Body)]], [Tag]), Args).

make_body_letrec(Variables, Args, Tmps, Body) ->
    from_let([ [V, ?UNASSIGNED] || V <- Variables ],
             [from_let([ [T, A] || {T, A} <- lists:zip(Tmps, Args) ],
                       [ make_set(V, T) || {V, T} <- lists:zip(Variables, Tmps) ] ++ Body)]).

make_body_letrecs(Variables, Args, Tmps, Body) ->
    Vs = flatten_variables(Variables),
    Ts = flatten_variables(Tmps),
    from_let([ [V, ?UNASSIGNED] || V <- Vs ],
             [make_body_letrecs1(Args, Ts, [ make_set(V, T) || {V, T} <- lists:zip(Vs, Ts) ] ++ Body)]).

make_body_letrecs1([A], [T], Body) ->
    from_let([[T, A]], Body);
make_body_letrecs1([A|Args], [T|Tmps], Body) ->
    from_let([[T, A]], [make_body_letrecs1(Args, Tmps, Body)]).

%%%----------------------------------------------------------------------
%%% Internal functions - iteration
%%%----------------------------------------------------------------------

%% do
from_do([Specs, [Test], Commands]) ->
    from_do(Specs, Test, [?FALSE], Commands);
from_do([Specs, [Test|Exps], Commands]) ->
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

%% parameterize

%%%----------------------------------------------------------------------
%%% Internal functions - exception handling
%%%----------------------------------------------------------------------

%% guard

%%%----------------------------------------------------------------------
%%% Internal functions - quasiquotation
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Internal functions - scan_out_internal_definitions
%%%----------------------------------------------------------------------

scan_out_internal_defines(Body) ->
    scan_out_internal_defines(Body, [], []).

scan_out_internal_defines([], Defines, Body) ->
    [make_letrecs(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_defines([['define'|H]|T], Defines, Body) ->
    scan_out_internal_defines(T, [define_to_binding(H)|Defines], Body);
scan_out_internal_defines([H|T], Defines, Body) ->
    scan_out_internal_defines(T, Defines, [H|Body]).

define_to_binding([Variable, Exp]) when not is_list(Variable) ->
    [Variable, Exp];
define_to_binding([[Variable|Formals]|Body]) when not is_list(Variable) ->
    [Variable, make_lambda(Formals, Body)].
