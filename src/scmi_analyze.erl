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

-module(scmi_analyze).

%% External exports
-export([analyze/1
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
-export_type([ana/0
             ]).

%% Internal imports
-import(scmi_analyze_primitive, [analyze_lambda/2
                                 , analyze_application/2
                                 , analyze_if/2
                                 , analyze_assignment/2
                                 , analyze_include/2
                                 , analyze_include_ci/2
                                 , analyze_include_lib/2
                                 , analyze_include_lib_ci/2
                                ]).

-import(scmi_analyze_derived, [analyze_cond/2
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
                              ]).

-import(scmi_analyze_syntax, [analyze_let_syntax/2
                              , analyze_letrec_syntax/2
                              , analyze_syntax_rules/2
                              , analyze_syntax_error/2
                             ]).

-import(scmi_analyze_program, [analyze_import/2
                               , analyze_define/2
                               , analyze_define_values/2
                               , analyze_define_syntax/2
                               , analyze_define_record_type/2
                               , analyze_define_library/2
                              ]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% analyze
-type ana() :: #ana{}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze(scm_any()) -> scm_any().
analyze(Exp) ->
    analyze(Exp, #ana{}).

-spec analyze(scm_any(), scmi_ana()) -> scm_any().
analyze(Exp, Ana) when is_number(Exp) ->
    analyze_self_evaluating(Exp, Ana);
analyze({Num, Den}=Exp, Ana) when is_number(Num), is_number(Den) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?PINF=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?NINF=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?PNAN=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?NNAN=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?NZER=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze({Complex, {_A, _B}}=Exp, Ana) when Complex==rectangular; Complex==polar ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, boolean) ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, bytevector) ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, character) ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, string) ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, vector) ->
    analyze_self_evaluating(Exp, Ana);
analyze(?UNASSIGNED=Exp, Ana) ->
    analyze_self_evaluating(Exp, Ana);
analyze(Exp, Ana) when is_atom(Exp) ->
    analyze_variable(Exp, Ana);
analyze({Sha1, Var}=Exp, Ana) when is_atom(Sha1), is_binary(Var) ->
    analyze_variable(Exp, Ana);
analyze(Exp, Ana) when is_reference(Exp) ->
    analyze_variable(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, label) ->
    analyze_label(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, labelref) ->
    analyze_labelref(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, quote) ->
    analyze_quote(Exp, Ana);
analyze(['lambda'|Exp], Ana) ->
    analyze_lambda(Exp, Ana);
analyze(['if'|Exp], Ana) ->
    analyze_if(Exp, Ana);
analyze(['set!'|Exp], Ana) ->
    analyze_assignment(Exp, Ana);
analyze(['include'|Exp], Ana) ->
    analyze_include(Exp, Ana);
analyze(['include-ci'|Exp], Ana) ->
    analyze_include_ci(Exp, Ana);
analyze(['include-lib'|Exp], Ana) ->
    analyze_include_lib(Exp, Ana);
analyze(['include-lib-ci'|Exp], Ana) ->
    analyze_include_lib_ci(Exp, Ana);
analyze(['cond'|Exp], Ana) ->
    analyze_cond(Exp, Ana);
analyze(['case'|Exp], Ana) ->
    analyze_case(Exp, Ana);
analyze(['and'|Exp], Ana) ->
    analyze_and(Exp, Ana);
analyze(['or'|Exp], Ana) ->
    analyze_or(Exp, Ana);
analyze(['when'|Exp], Ana) ->
    analyze_when(Exp, Ana);
analyze(['unless'|Exp], Ana) ->
    analyze_unless(Exp, Ana);
analyze(['cond-expand'|Exp], Ana) ->
    analyze_cond_expand(Exp, Ana);
analyze(['let'|Exp], Ana) ->
    analyze_let(Exp, Ana);
analyze(['let*'|Exp], Ana) ->
    analyze_lets(Exp, Ana);
analyze(['letrec'|Exp], Ana) ->
    analyze_letrec(Exp, Ana);
analyze(['letrec*'|Exp], Ana) ->
    analyze_letrecs(Exp, Ana);
analyze(['let-values'|Exp], Ana) ->
    analyze_let_values(Exp, Ana);
analyze(['let*-values'|Exp], Ana) ->
    analyze_lets_values(Exp, Ana);
analyze(['letrec-values'|Exp], Ana) ->
    analyze_letrec_values(Exp, Ana);
analyze(['begin'|Exp], Ana) ->
    analyze_begin(Exp, Ana);
analyze(['do'|Exp], Ana) ->
    analyze_do(Exp, Ana);
analyze(['make-parameter'|Exp], Ana) ->
    analyze_make_parameter(Exp, Ana);
analyze(['parameterize'|Exp], Ana) ->
    analyze_parameterize(Exp, Ana);
analyze(['guard'|Exp], Ana) ->
    analyze_guard(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, quasiquote) ->
    analyze_quasiquote(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, unquote) ->
    analyze_unquote(Exp, Ana);
analyze(Exp, Ana) when is_record(Exp, unquote_splicing) ->
    analyze_unquote_splicing(Exp, Ana);
analyze(['let-syntax'|Exp], Ana) ->
    analyze_let_syntax(Exp, Ana);
analyze(['letrec-syntax'|Exp], Ana) ->
    analyze_letrec_syntax(Exp, Ana);
analyze(['syntax-rules'|Exp], Ana) ->
    analyze_syntax_rules(Exp, Ana);
analyze(['syntax-error'|Exp], Ana) ->
    analyze_syntax_error(Exp, Ana);
analyze(['import'|Exp], Ana) ->
    analyze_import(Exp, Ana);
analyze(['define'|Exp], Ana) ->
    analyze_define(Exp, Ana);
analyze(['define-values'|Exp], Ana) ->
    analyze_define_values(Exp, Ana);
analyze(['define-syntax'|Exp], Ana) ->
    analyze_define_syntax(Exp, Ana);
analyze(['define-record-type'|Exp], Ana) ->
    analyze_define_record_type(Exp, Ana);
analyze(['define-library'|Exp], Ana) ->
    analyze_define_library(Exp, Ana);
analyze([_H|T]=Exp, Ana) when is_list(T) ->
    analyze_application(Exp, Ana);
analyze(Exp, Ana) ->
    erlang:error(badarg, [Exp, Ana]).

-spec classify(scm_any()) -> atom() | {rectangular | polar, {atom(), atom()}}.
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
    {Complex, {classify(A), classify(B)}};
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
classify(Exp) when is_record(Exp, quote) ->
    quote;
classify(['lambda'|_]) ->
    lambda;
classify(['if'|_]) ->
    'if';
classify(['set!'|_]) ->
    assignment;
classify(['include'|_]) ->
    include;
classify(['include-ci'|_]) ->
    include_ci;
classify(['include-lib'|_]) ->
    include_lib;
classify(['include-lib-ci'|_]) ->
    include_lib_ci;
classify(['cond'|_]) ->
    'cond';
classify(['case'|_]) ->
    'case';
classify(['and'|_]) ->
    'and';
classify(['or'|_]) ->
    'or';
classify(['when'|_]) ->
    'when';
classify(['unless'|_]) ->
    'unless';
classify(['cond-expand'|_]) ->
    cond_expand;
classify(['let'|_]) ->
    'let';
classify(['let*'|_]) ->
    lets;
classify(['letrec'|_]) ->
    letrec;
classify(['letrec*'|_]) ->
    letrecs;
classify(['let-values'|_]) ->
    let_values;
classify(['let*-values'|_]) ->
    lets_values;
classify(['letrec-values'|_]) ->
    letrec_values;
classify(['begin'|_]) ->
    'begin';
classify(['do'|_]) ->
    do;
classify(['make-parameter'|_]) ->
    make_parameter;
classify(['parameterize'|_]) ->
    parameterize;
classify(['guard'|_]) ->
    guard;
classify(Exp) when is_record(Exp, quasiquote) ->
    quasiquote;
classify(Exp) when is_record(Exp, unquote) ->
    unquote;
classify(Exp) when is_record(Exp, unquote_splicing) ->
    unquote_splicing;
classify(['let-syntax'|_]) ->
    let_syntax;
classify(['letrec-syntax'|_]) ->
    letrec_syntax;
classify(['syntax-rules'|_]) ->
    syntax_rules;
classify(['syntax-error'|_]) ->
    syntax_error;
classify(['import'|_]) ->
    import;
classify(['define'|_]) ->
    define;
classify(['define-values'|_]) ->
    define_values;
classify(['define-syntax'|_]) ->
    define_syntax;
classify(['define-record-type'|_]) ->
    define_record_type;
classify(['define-library'|_]) ->
    define_library;
classify([_H|T]) when is_list(T) ->
    application;
classify([]) ->
    nil;
classify(Exp) ->
    erlang:error(badarg, [Exp]).

-spec is_reserved_symbol(scmi_var()) -> boolean().
is_reserved_symbol(?UNASSIGNED) -> true;
is_reserved_symbol('quote') -> true;
is_reserved_symbol('lambda') -> true;
is_reserved_symbol('if') -> true;
is_reserved_symbol('set!') -> true;
is_reserved_symbol('include') -> true;
is_reserved_symbol('include-ci') -> true;
is_reserved_symbol('include-lib') -> true;
is_reserved_symbol('include-lib-ci') -> true;
is_reserved_symbol('cond') -> true;
is_reserved_symbol('case') -> true;
is_reserved_symbol('and') -> true;
is_reserved_symbol('or') -> true;
is_reserved_symbol('when') -> true;
is_reserved_symbol('unless') -> true;
is_reserved_symbol('cond-expand') -> true;
is_reserved_symbol('let') -> true;
is_reserved_symbol('let*') -> true;
is_reserved_symbol('letrec') -> true;
is_reserved_symbol('letrec*') -> true;
is_reserved_symbol('let-values') -> true;
is_reserved_symbol('let*-values') -> true;
is_reserved_symbol('letrec-values') -> true;
is_reserved_symbol('begin') -> true;
is_reserved_symbol('do') -> true;
is_reserved_symbol('make-parameter') -> true;
is_reserved_symbol('parameterize') -> true;
is_reserved_symbol('guard') -> true;
is_reserved_symbol('quasiquote') -> true;
is_reserved_symbol('unquote') -> true;
is_reserved_symbol('unquote-splicing') -> true;
is_reserved_symbol('let-syntax') -> true;
is_reserved_symbol('letrec-syntax') -> true;
is_reserved_symbol('syntax-rules') -> true;
is_reserved_symbol('syntax-error') -> true;
is_reserved_symbol('import') -> true;
is_reserved_symbol('define') -> true;
is_reserved_symbol('define-values') -> true;
is_reserved_symbol('define-syntax') -> true;
is_reserved_symbol('define-record-type') -> true;
is_reserved_symbol('define-library') -> true;
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

-spec splitnv_arguments(pos_integer(), [scm_any(),...]) -> [scm_any(),...].
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

analyze_label(Exp, _Ana) ->
    erlang:error(unsupported, [Exp]).

analyze_labelref(Exp, _Ana) ->
    erlang:error(unsupported, [Exp]).

analyze_quote(#quote{val=Exp}, _Ana) ->
    fun(_Env, Ok, Ng) -> Ok(Exp, Ng) end.

analyze_self_evaluating(Exp, _Ana) ->
    fun(_Env, Ok, Ng) -> Ok(Exp, Ng) end.

analyze_variable(Exp, _Ana) ->
    fun(Env, Ok, Ng) -> Ok(scmi_env:lookup_variable(Exp, Env), Ng) end.
