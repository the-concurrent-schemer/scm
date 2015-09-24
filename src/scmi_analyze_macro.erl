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

%%% @doc Scheme interpreter syntactic analyzer for syntax expressions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze_macro).

%% SCMI Exports
-export(['$scmi_exports'/0]).

%% External exports
-export([analyze_lambda_syntax/2
        , analyze_assignment_syntax/2
        , analyze_sequence_syntax/2
        , analyze_let_syntax/2
        , analyze_lets_syntax/2
        , analyze_letrec_syntax/2
        , analyze_letrecs_syntax/2
        , analyze_syntax_rules/2
        , analyze_syntax_error/2
        , scan_out_internal_definitions/2
        ]).

-import(scmi_analyze, [analyze/2, validate_variables/1, validate_variable/1, make_tmp_variables/1]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-define(ELLIPSIS, '...').
-define(UNDERSCORE, '_').

-record(plid, {defref :: reference(), val}). % pattern literal identifier
-record(pvar, {useref :: reference(), val}). % pattern variable
-record(any,  {useref :: reference(), val}). % match any element
-record(star, {useref :: reference(), val}). % match zero or more elements

-record(state, {
          elp=?UNASSIGNED   % ellipsis
               , uns=?UNASSIGNED % underscore
               , ls=[]           % pattern literal identifiers
               , vs=[]           % pattern variables
         }).


%%%----------------------------------------------------------------------
%%% SCMI Exports
%%%----------------------------------------------------------------------

-spec '$scmi_exports'() -> [{scm_symbol(), scmi_expander()}].
'$scmi_exports'() ->
    [{'lambda-syntax', #expander{val=fun ?MODULE:'analyze_lambda_syntax'/2}}
    , {'set!-syntax', #expander{val=fun ?MODULE:'analyze_assignment_syntax'/2}}
    , {'begin-syntax', #expander{val=fun ?MODULE:'analyze_sequence_syntax'/2}}
    , {'let-syntax', #expander{val=fun ?MODULE:'analyze_let_syntax'/2}}
    , {'let*-syntax', #expander{val=fun ?MODULE:'analyze_lets_syntax'/2}}
    , {'letrec-syntax', #expander{val=fun ?MODULE:'analyze_letrec_syntax'/2}}
    , {'letrec*-syntax', #expander{val=fun ?MODULE:'analyze_letrecs_syntax'/2}}
    , {'syntax-rules', #expander{val=fun ?MODULE:'analyze_syntax_rules'/2}}
    , {'syntax-error', #expander{val=fun ?MODULE:'analyze_syntax_error'/2}}
    ].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_lambda_syntax(scmi_exp(), scmi_senv()) -> scmi_expander().
analyze_lambda_syntax([Parameters|Body], #senv{env=BaseEnv}=BaseSEnv) when is_list(Parameters) ->
    validate_variables(Parameters),
    Body1 = scan_out_internal_definitions(Body, BaseSEnv),
    Fun = fun(Args, SEnv) ->
                  analyze_sequence_syntax(Body1, SEnv#senv{env=scmi_env:extend(Parameters, Args, BaseEnv)})
          end,
    #expander{val=Fun}.

-spec analyze_assignment_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_assignment_syntax([Variable, Exp]=Exp0, #senv{env=Env}=SEnv) ->
    validate_variable(Variable), % validate variable
    case analyze(Exp, SEnv) of
        #expander{defref=DefRef}=Val ->
            scmi_env:set_variable(Variable, Val, Env),
            fun(DefEnv, Ok, Ng) ->
                    %% define marker for macro identifiers
                    scmi_env:define_variable(DefRef, ?UNASSIGNED, DefEnv),
                    Ok(?FALSE, Ng) end;
        _ ->
            erlang:error(badarg, [Exp0, SEnv])
    end.

-spec analyze_sequence_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_sequence_syntax([]=Exp, SEnv) ->
    erlang:error(badarg, [Exp, SEnv]);
analyze_sequence_syntax([Exp], SEnv) ->
    analyze(Exp, SEnv);
analyze_sequence_syntax([Exp|Exps], SEnv) ->
    _ = analyze(Exp, SEnv),
    analyze_sequence_syntax(Exps, SEnv).

-spec analyze_let_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_let_syntax(Exp, SEnv) ->
    analyze(expand_let_syntax(Exp, SEnv), SEnv).

-spec analyze_lets_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_lets_syntax(Exp, SEnv) ->
    analyze(expand_lets_syntax(Exp, SEnv), SEnv).

-spec analyze_letrec_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_letrec_syntax(Exp, SEnv) ->
    analyze(expand_letrec_syntax(Exp, SEnv), SEnv).

-spec analyze_letrecs_syntax(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_letrecs_syntax(Exp, SEnv) ->
    analyze(expand_letrecs_syntax(Exp, SEnv), SEnv).

-spec analyze_syntax_rules(scmi_exp(), scmi_senv()) -> scmi_expander().
analyze_syntax_rules([Ellipsis, Literals|Rules], #senv{env=DefEnv}=DefSEnv) when not is_list(Ellipsis), is_list(Literals), is_list(Rules) ->
    Rules1 = validate_rules(Ellipsis, Literals, Rules),
    DefRef = make_variable(),
    Fun = fun(Args, SEnv) ->
                  {_N, Bindings, Template} = match_rules(Args, Rules1),
                  UseRef = make_variable(),
                  Exp = expand_template(DefSEnv, DefRef, UseRef, Bindings, Template),
                  analyze(Exp, SEnv)
          end,
    %% define marker for macro identifiers
    scmi_env:define_variable(DefRef, ?UNASSIGNED, DefEnv),
    #expander{defref=DefRef, val=Fun};
analyze_syntax_rules([Ellipsis, Literals], SEnv) when not is_list(Ellipsis), is_list(Literals) ->
    analyze_syntax_rules([Ellipsis, Literals, []], SEnv);
analyze_syntax_rules([Literals|Rules], SEnv) when is_list(Literals), is_list(Rules) ->
    analyze_syntax_rules([?ELLIPSIS, Literals|Rules], SEnv);
analyze_syntax_rules(Exp, SEnv) ->
    erlang:error(badarg, [Exp, SEnv]).

-spec analyze_syntax_error(scmi_exp(), scmi_senv()) -> no_return().
analyze_syntax_error(Exp, SEnv) ->
    erlang:error(scm_syntax_error, [Exp, SEnv]).

-spec scan_out_internal_definitions([scmi_exp(),...], scmi_senv()) -> [scmi_exp()].
scan_out_internal_definitions(Body, #senv{env=Env}) ->
    FunDS = fun scmi_analyze_program:'analyze_define_syntax'/2,

    HaveFunDS = case scmi_env:safe_lookup_variable('define-syntax', Env) of
                    #expander{val=FunDS} ->
                        true;
                    _ ->
                        false
                end,

    Fun = fun(['define-syntax'|_], none) when HaveFunDS ->
                  define_syntax;
             (_, Acc) ->
                  Acc
          end,
    case lists:foldl(Fun, none, Body) of
        define_syntax ->
            scan_out_internal_define_syntax(Body);
        none ->
            Body
    end.

%%%----------------------------------------------------------------------
%%% Internal functions - let-syntax derived binding constructs
%%%----------------------------------------------------------------------

%% let-syntax
expand_let_syntax([[]|Body], SEnv) ->
    make_begin_syntax(scan_out_internal_definitions(Body, SEnv));
expand_let_syntax([Bindings|Body], SEnv) ->
    from_let_syntax(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let*-syntax
expand_lets_syntax([[]|Body], SEnv) ->
    make_begin_syntax(scan_out_internal_definitions(Body, SEnv));
expand_lets_syntax([Bindings|Body], SEnv) ->
    from_lets_syntax(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% letrec-syntax
expand_letrec_syntax([[]|Body], SEnv) ->
    make_begin_syntax(scan_out_internal_definitions(Body, SEnv));
expand_letrec_syntax([Bindings|Body], SEnv) ->
    from_letrec_syntax(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% letrec*-syntax
expand_letrecs_syntax([[]|Body], SEnv) ->
    make_begin_syntax(scan_out_internal_definitions(Body, SEnv));
expand_letrecs_syntax([Bindings|Body], SEnv) ->
    from_letrecs_syntax(Bindings, scan_out_internal_definitions(Body, SEnv)).

%% let-syntax
from_let_syntax(Bindings, Body) ->
    from_let_syntax(Bindings, Body, [], []).

from_let_syntax([[Variable, Init]], Body, Variables, Inits) ->
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    make_body_let_syntax(Vs, Is, Body);
from_let_syntax([[Variable, Init]|Bindings], Body, Variables, Inits) ->
    from_let_syntax(Bindings, Body, [Variable|Variables], [Init|Inits]).

%% let*-syntax
from_lets_syntax([_Binding]=Bindings, Body) ->
    from_let_syntax(Bindings, Body);
from_lets_syntax([Binding|Bindings], Body) ->
    from_let_syntax([Binding], [from_lets_syntax(Bindings, Body)]).

%% letrec-syntax and letrec*-syntax
from_letrec_syntax(Bindings, Body) ->
    from_letrec_syntax(fun make_let_syntax/2, Bindings, Body, [], [], []).

from_letrecs_syntax(Bindings, Body) ->
    from_letrec_syntax(fun make_lets_syntax/2, Bindings, Body, [], [], []).

from_letrec_syntax(Make, [[Variable, Init]], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    Vs = lists:reverse([Variable|Variables]),
    Is = lists:reverse([Init|Inits]),
    Ts = lists:reverse([Tmp|Tmps]),
    make_body_letrec_syntax(Make, Vs, Is, Ts, Body);
from_letrec_syntax(Make, [[Variable, Init]|Bindings], Body, Variables, Inits, Tmps) ->
    Tmp = make_tmp_variables(Variable),
    from_letrec_syntax(Make, Bindings, Body, [Variable|Variables], [Init|Inits], [Tmp|Tmps]).

%% let-syntax, letrec-syntax, and letrec*-syntax helpers
make_body_let_syntax(Variables, Args, Body) ->
    make_call(make_lambda_syntax(Variables, Body), Args).

make_body_letrec_syntax(Make, Variables, Args, Tmps, Body) ->
    make_let_syntax([ [V, ?UNASSIGNED] || V <- Variables ],
                    [Make([ [T, A] || {T, A} <- lists:zip(Tmps, Args) ],
                          [ make_setb_syntax(V, T) || {V, T} <- lists:zip(Variables, Tmps) ] ++ Body)]).

%%%----------------------------------------------------------------------
%%% Internal functions - syntax-rules pattern language
%%%----------------------------------------------------------------------

validate_rules(Ellipsis, Literals, Rules) ->
    State =
        case {lists:member(Ellipsis, Literals), lists:member(?UNDERSCORE, Literals)} of
            {false, false} ->
                #state{elp=Ellipsis, uns=?UNDERSCORE};
            {false, true} ->
                #state{elp=Ellipsis};
            {true, false} ->
                #state{uns=?UNDERSCORE};
            {true, true} ->
                #state{}
        end,
    validate_rules1(State#state{ls=Literals}, Rules).

validate_rules1(#state{elp=Ellipsis, ls=Ls}=State, Rules) ->
    validate_variables([Ellipsis|Ls]), % validate variables
    validate_rules1(State, Rules, []).

validate_rules1(_State, [], Acc) ->
    lists:reverse(Acc);
validate_rules1(State, [Rule|Rules], Acc) ->
    {_State, Rule1} = validate_rule(State, Rule),
    validate_rules1(State, Rules, [Rule1|Acc]).

validate_rule(State, [Pattern, Template]) ->
    {State1, Pattern1} = validate_srpattern(State, Pattern),
    {State2, Template1} = validate_template(State1, Template),
    {State2, [Pattern1, Template1]};
validate_rule(State, Rule) ->
    erlang:error(badarg, [State, Rule]).

validate_srpattern(State, [[Identifier|PatternL]|PatternR]) ->
    _ = validate_pattern_identifier(State, Identifier), % ignored
    validate_pattern(State, [[PatternL]|PatternR]);
validate_srpattern(State, [Identifier|Pattern]) ->
    _ = validate_pattern_identifier(State, Identifier), % ignored
    validate_pattern_list(State, Pattern);
validate_srpattern(State, Pattern) ->
    erlang:error(badarg, [State, Pattern]).

validate_pattern_identifier(#state{elp=Ellipsis}=State, Ellipsis) ->
    erlang:error(badarg, [State, Ellipsis]);
validate_pattern_identifier(#state{uns=Underscore}=State, Underscore) ->
    validate_variable(Underscore), % validate variable
    {State, #any{val=Underscore}};
validate_pattern_identifier(#state{ls=Ls, vs=Vs}=State, Identifier) ->
    validate_variable(Identifier),  % validate variable
    case lists:member(Identifier, Ls) of
        true ->
            {State, #plid{val=Identifier}};
        false ->
            case lists:member(Identifier, Vs) of
                true ->
                    erlang:error(badarg, [State, Identifier]);
                false ->
                    {State#state{vs=[Identifier|Vs]}, #pvar{val=Identifier}}
            end
    end.

validate_pattern_vector(State, Pattern) ->
    {State1, Pattern1} = validate_pattern_list(State, tuple_to_list(Pattern)),
    {State1, list_to_tuple(Pattern1)}.

validate_pattern_list(State, Pattern) ->
    validate_pattern_list(State, Pattern, false, []).

validate_pattern_list(State, [], _HasEllipsis, Acc) ->
    {State, lists:reverse(Acc)};
validate_pattern_list(State, Pattern, _HasEllipsis, Acc) when not is_list(Pattern) ->
    {State1, Pattern1} = validate_pattern(State, Pattern),
    {State1, [lists:reverse(Acc)|Pattern1]};
validate_pattern_list(#state{elp=Ellipsis}=State, [Ellipsis|_L]=Pattern0, true=HasEllipsis, Acc) ->
    erlang:error(badarg, [State, Pattern0, HasEllipsis, Acc]);
validate_pattern_list(#state{elp=Ellipsis}=State, [Ellipsis|_L]=Pattern0, false=HasEllipsis, Acc) when length(Acc) < 1 ->
    erlang:error(badarg, [State, Pattern0, HasEllipsis, Acc]);
validate_pattern_list(#state{elp=Ellipsis}=State, [Ellipsis|L], false, [Pattern|Acc]) ->
    validate_pattern_list(State, L, true, [#star{val=Pattern}|Acc]);
validate_pattern_list(State, [Pattern|L], HasEllipsis, Acc) ->
    {State1, Pattern1} = validate_pattern(State, Pattern),
    validate_pattern_list(State1, L, HasEllipsis, [Pattern1|Acc]).

validate_pattern(State, #vector{val=Pattern}) ->
    {State1, Pattern1} = validate_pattern_vector(State, Pattern),
    {State1, #vector{val=Pattern1}};
validate_pattern(State, Pattern) when is_list(Pattern) ->
    validate_pattern_list(State, Pattern);
validate_pattern(State, Pattern) ->
    case scmi_analyze:classify(Pattern) of
        string ->
            {State, Pattern};
        character ->
            {State, Pattern};
        boolean ->
            {State, Pattern};
        Number when Number==integer;
                    Number==float;
                    Number==rational;
                    Number==pinf;
                    Number==ninf;
                    Number==pnan;
                    Number==nnan;
                    Number==nzer ->
            {State, Pattern};
        {Complex, {_A, _B}} when Complex==rectangular; Complex==polar ->
            {State, Pattern};
        bytevector ->
            {State, Pattern};
        Class when Class==identifier; Class==variable ->
            validate_pattern_identifier(State, Pattern);
        _ ->
            erlang:error(badarg, [State, Pattern])
    end.

validate_template_identifier(#state{elp=Ellipsis}=State, Ellipsis) ->
    erlang:error(badarg, [State, Ellipsis]);
validate_template_identifier(#state{uns=Underscore}=State, Underscore) ->
    validate_variable(Underscore), % validate variable
    {State, Underscore};
validate_template_identifier(#state{vs=Vs}=State, Identifier) ->
    validate_variable(Identifier),  % validate variable
    case lists:member(Identifier, Vs) of
        true ->
            {State, #pvar{val=Identifier}};
        false ->
            {State, #mid{val=Identifier}}
    end.

validate_template_vector(State, Template) ->
    {State1, Template1} = validate_template_list(State, tuple_to_list(Template)),
    {State1, list_to_tuple(Template1)}.

validate_template_list(#state{elp=Ellipsis}=State, [Ellipsis, Ellipsis]) ->
    {State, Ellipsis};
validate_template_list(#state{elp=Ellipsis}=State, [Ellipsis, Template]) ->
    validate_template(State#state{elp=?UNASSIGNED}, Template);
validate_template_list(State, Template) ->
    validate_template_list(State, Template, []).

validate_template_list(State, [], Acc) ->
    {State, lists:reverse(Acc)};

validate_template_list(State, Template, Acc) when not is_list(Template) ->
    {State1, Template1} = validate_template(State, Template),
    {State1, [lists:reverse(Acc)|Template1]};
validate_template_list(#state{elp=Ellipsis}=State, [Template, Ellipsis|L], Acc) ->
    {State1, Template1} = validate_template(State, Template),
    validate_template_list(State1, L, [#star{val=Template1}|Acc]);
validate_template_list(State, [Template|L], Acc) ->
    {State1, Template1} = validate_template(State, Template),
    validate_template_list(State1, L, [Template1|Acc]).

validate_template(State, #vector{val=Template}) ->
    {State1, Template1} = validate_template_vector(State, Template),
    {State1, #vector{val=Template1}};
validate_template(State, Template) when is_list(Template) ->
    validate_template_list(State, Template);
validate_template(State, Template) ->
    case scmi_analyze:classify(Template) of
        string ->
            {State, Template};
        character ->
            {State, Template};
        boolean ->
            {State, Template};
        Number when Number==integer;
                    Number==float;
                    Number==rational;
                    Number==pinf;
                    Number==ninf;
                    Number==pnan;
                    Number==nnan;
                    Number==nzer ->
            {State, Template};
        {Complex, {_A, _B}} when Complex==rectangular; Complex==polar ->
            {State, Template};
        bytevector ->
            {State, Template};
        Class when Class==identifier; Class==variable ->
            validate_template_identifier(State, Template);
        _ ->
            erlang:error(badarg, [State, Template])
    end.

match_rules(Args, Rules) ->
    match_rules1(Args, Rules, 1).

match_rules1(Args, []=Rules, N) ->
    erlang:error(badarg, [Args, Rules, N]);
match_rules1(Args, [Rule|Rules], N) ->
    case match_srpattern(Args, Rule) of
        none ->
            match_rules1(Args, Rules, N+1);
        {Bindings, Template} ->
            {N, Bindings, Template}
    end.

match_srpattern(Args, [Pattern, Template]) ->
    case match_pattern_list({0,[]}, Args, Pattern) of
        none ->
            none;
        Bindings ->
            {Bindings, Template}
    end.

match_pattern_vector(DI, E, P) ->
    match_pattern_list(DI, tuple_to_list(E), tuple_to_list(P)).

match_pattern_list(DI, E, P) ->
    match_pattern_list(DI, E, P, []).

match_pattern_list(_DI, [], [], Bindings) ->
    lists:reverse(Bindings);
match_pattern_list(DI, Es, [#star{val=P}|Ps], Bindings) when is_list(Es) ->
    case match_pattern_star(DI, Es, P) of
        {_Es1, []} ->
            match_pattern_list(DI, Es, Ps, Bindings);
        {Es1, Bindings1} ->
            match_pattern_list(DI, Es1, Ps, Bindings1++Bindings)
    end;
match_pattern_list(DI, E, P, Bindings) when not is_list(P) ->
    match_pattern(DI, E, P, Bindings);
match_pattern_list(DI, [E|Es], [P|Ps], Bindings) ->
    case match_pattern(DI, E, P, Bindings) of
        none ->
            none;
        Bindings1 ->
            match_pattern_list(DI, Es, Ps, Bindings1)
    end;
match_pattern_list(_DI, _E, _P, _Bindings) ->
    none.

match_pattern_star({D,I}, Es, P) ->
    match_pattern_star(D+1, I, 0, Es, P, []).

match_pattern_star(_D, _I, _J, []=Es, _P, Bindings) ->
    {Es, lists:reverse(Bindings)};
match_pattern_star(D, I, J, [E|Es]=Es0, P, Bindings) ->
    case match_pattern({D,[J|I]}, E, P, Bindings) of
        none ->
            {Es0, Bindings};
        Bindings1 ->
            match_pattern_star(D, I, J+1, Es, P, Bindings1)
    end.

match_pattern(DI, #vector{val=E}, #vector{val=P}, Bindings) ->
    case match_pattern_vector(DI, E, P) of
        none ->
            none;
        Bindings1 ->
            Bindings1++Bindings
    end;
match_pattern(DI, E, P, Bindings) when is_list(P) ->
    case match_pattern_list(DI, E, P) of
        none ->
            none;
        Bindings1 ->
            Bindings1++Bindings
    end;
match_pattern(_DI, _E, #any{}, Bindings) ->
    Bindings;
match_pattern(DI, E, #pvar{}=P, Bindings) ->
    [{{P, DI}, E}|Bindings];
match_pattern(_DI, E, #plid{val=P}, Bindings) ->
    %% CAUTION: This is an oversimplification of "with the same binding"
    case scml_base_equality:'equal?'(E, P) of
        ?FALSE ->
            none;
        _ ->
            Bindings
    end;
match_pattern(_DI, E, P, Bindings) ->
    %% CAUTION: 'equal?'/2 is hard-coded
    case scml_base_equality:'equal?'(E, P) of
        ?FALSE ->
            none;
        _ ->
            Bindings
    end.

expand_template(DefSEnv, DefRef, UseRef, Bindings, Template) ->
    expand_template({0,[]}, DefSEnv, DefRef, UseRef, Bindings, Template).

expand_template_identifier(_DI, #senv{env=DefEnv}, DefRef, UseRef, _Bindings, #mid{val=Identifier}=Template) ->
    case scmi_env:safe_lookup_variable(Identifier, DefEnv) of
        ?UNASSIGNED ->
            make_variable(UseRef, Identifier);
        _ ->
            Template#mid{defref=DefRef}
    end.

expand_template_vector(DI, DefSEnv, DefRef, UseRef, Bindings, Template) ->
    Exp = expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, tuple_to_list(Template)),
    list_to_tuple(Exp).

expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, Template) ->
    expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, Template, []).

expand_template_list(_DI, _DefSEnv, _DefRef, _UseRef, _Bindings, [], Acc) ->
    lists:reverse(Acc);
expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, Template, Acc) when not is_list(Template) ->
    Exp = expand_template(DI, DefSEnv, DefRef, UseRef, Bindings, Template),
    [lists:reverse(Acc)|Exp];
expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, [#star{val=Template}|L], Acc) ->
    Exp = expand_template_star(DI, DefSEnv, DefRef, UseRef, Bindings, Template),
    expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, L, Exp++Acc);
expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, [Template|L], Acc) ->
    Exp = expand_template(DI, DefSEnv, DefRef, UseRef, Bindings, Template),
    expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, L, [Exp|Acc]).

expand_template_star({D, I}, DefSEnv, DefRef, UseRef, Bindings, Template) ->
    expand_template_star(D+1, I, 0, DefSEnv, DefRef, UseRef, Bindings, Template, []).

expand_template_star(D, I, J, DefSEnv, DefRef, UseRef, Bindings, Template, Acc) ->
    %% @TODO replace this mechanism with a state-based approached that
    %% can detect invalid macro definitions and/or macro uses.
    try
        Exp = expand_template({D,[J|I]}, DefSEnv, DefRef, UseRef, Bindings, Template),
        expand_template_star(D, I, J+1, DefSEnv, DefRef, UseRef, Bindings, Template, [Exp|Acc])
    catch
        throw:{no_binding, _}=_Y ->
            %% DEBUG io:format("~p~n~p~n", [[D, I, J, DefSEnv, DefRef, UseRef, Bindings, Template, Acc], _Y]),
            Acc
    end.

expand_template(DI, DefSEnv, DefRef, UseRef, Bindings, #vector{val=Template}) ->
    Exp = expand_template_vector(DI, DefSEnv, DefRef, UseRef, Bindings, Template),
    #vector{val=Exp};
expand_template(DI, DefSEnv, DefRef, UseRef, Bindings, Template) when is_list(Template) ->
    expand_template_list(DI, DefSEnv, DefRef, UseRef, Bindings, Template);
expand_template(DI, _DefSEnv, _DefRef, _UseRef, Bindings, #pvar{}=Template) ->
    lookup_template_binding(DI, Template, Bindings);
expand_template(DI, DefSEnv, DefRef, UseRef, Bindings, Template) ->
    case scmi_analyze:classify(Template) of
        string ->
            Template;
        character ->
            Template;
        boolean ->
            Template;
        Number when Number==integer;
                    Number==float;
                    Number==rational;
                    Number==pinf;
                    Number==ninf;
                    Number==pnan;
                    Number==nnan;
                    Number==nzer ->
            Template;
        {Complex, {_A, _B}} when Complex==rectangular; Complex==polar ->
            Template;
        bytevector ->
            Template;
        Class when Class==identifier; Class==variable ->
            expand_template_identifier(DI, DefSEnv, DefRef, UseRef, Bindings, Template);
        _ ->
            erlang:error(badarg, [DI, DefSEnv, DefRef, UseRef, Bindings, Template])
    end.

lookup_template_binding(DI, Identifier, Bindings) ->
    case proplists:get_value({Identifier, DI}, Bindings, ?UNASSIGNED) of
        ?UNASSIGNED ->
            erlang:throw({no_binding, [DI, Identifier, Bindings]});
        Value ->
            Value
    end.

%%%----------------------------------------------------------------------
%%% Internal functions - scan_out_internal_definitions
%%%----------------------------------------------------------------------

scan_out_internal_define_syntax(Body) ->
    scan_out_internal_define_syntax(Body, [], []).

scan_out_internal_define_syntax([], Defines, Body) ->
    [make_letrecs_syntax(lists:reverse(Defines), lists:reverse(Body))];
scan_out_internal_define_syntax([['define-syntax'|H]|T], Defines, Body) ->
    scan_out_internal_define_syntax(T, [define_syntax_to_binding(H)|Defines], Body);
scan_out_internal_define_syntax([H|T], Defines, Body) ->
    scan_out_internal_define_syntax(T, Defines, [H|Body]).

%% (define-syntax <variable> <expression>)
define_syntax_to_binding([Variable, Exp]) when not is_list(Variable) ->
    [Variable, Exp].
