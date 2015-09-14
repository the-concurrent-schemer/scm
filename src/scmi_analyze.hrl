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

-ifndef(scmi_analyze).
-define(scmi_analyze, true).

-include("scmi.hrl").

-record(senv, {
          env     :: scmi_env:env(),                    % Syntactic environment
          file    :: undefined | scmd_parse:filename(), % Current file
          path=[] :: [scmd_parse:filename()]            % Include-path
         }).

%% Default imports
-compile({nowarn_unused_function,
          [make_and/1
           , make_apply/2
           , make_assoc/2, make_assoc/3
           , make_begin/1
           , make_begin_syntax/1
           , make_booleanp/1
           , make_booleansp/1
           , make_bytevector_length/1, make_bytevector_ref/1
           , make_bytevectorsp/1
           , make_call/1, make_call/2
           , make_call_with_values/2
           , make_callcc/1
           , make_car/1, make_cdr/1, make_caar/1, make_cadr/1, make_cdar/1, make_cddr/1
           , make_case/2, make_case/4
           , make_charsp/1
           , make_char_to_integer/1, make_integer_to_char/1
           , make_cond/1, make_cond/2
           , make_cond_expand/1, make_cond_expand/2
           , make_cons/2
           , make_define/2
           , make_define_values/2
           , make_do/2, make_do/3, make_do/4
           , make_dynamic_wind/3
           , make_eqvp/2, make_eqp/2, make_equalp/2
           , make_error/1, make_error/2
           , make_else/1
           , make_foreach/2
           , make_if/2, make_if/3
           , make_guard/3, make_guard/4
           , make_lambda/2
           , make_lambda_syntax/2
           , make_list/1
           , make_let/2
           , make_let_named/3
           , make_lets/2
           , make_letrec/2
           , make_letrecs/2
           , make_let_values/2
           , make_lets_values/2
           , make_letrec_values/2
           , make_let_syntax/2
           , make_lets_syntax/2
           , make_letrec_syntax/2
           , make_letrecs_syntax/2
           , make_map/2
           , make_member/2, make_member/3
           , make_not/1
           , make_nullp/1
           , make_or/1
           , make_parameter/1, make_parameter/2
           , make_parameterize/2
           , make_quote/1
           , make_raise/1
           , make_raise_continuable/1
           , make_setb/2
           , make_setb_syntax/2
           , make_string_length/1, make_string_ref/1
           , make_stringsp/1
           , make_string_to_list/1, make_string_to_list/2, make_string_to_list/3, make_list_to_string/1
           , make_string_to_utf8/1, make_string_to_utf8/2, make_string_to_utf8/3, make_utf8_to_string/1, make_utf8_to_string/2, make_utf8_to_string/3
           , make_string_map/2
           , make_string_foreach/2
           , make_symbolsp/1
           , make_symbol_to_string/1, make_string_to_symbol/1
           , make_thunk/1
           , make_unless/2
           , make_values/1
           , make_variable/0, make_variable/1, make_variable/2
           , make_vector_length/1, make_vector_ref/1
           , make_vectorsp/1
           , make_vector_to_list/1, make_vector_to_list/2, make_vector_to_list/3, make_list_to_vector/1
           , make_vector_to_string/1, make_vector_to_string/2, make_vector_to_string/3, make_string_to_vector/1
           , make_vector_map/2
           , make_vector_foreach/2
           , make_when/2
           , make_with_exception_handler/2
          ]}).

%% @TBD
%% -compile({inline,
%%           [
%%           ]}).

make_and(Exps) when is_list(Exps) ->
    ['and'|Exps].

make_apply(Proc, Args) when is_list(Args) ->
    ['apply'|[Proc|Args]].

make_assoc(Obj, Alist) when is_list(Alist) ->
    ['assoc', Obj, Alist].

make_assoc(Obj, Alist, Compare) when is_list(Alist) ->
    ['assoc', Obj, Alist, Compare].

make_begin(Exps) when is_list(Exps) ->
    ['begin'|Exps].

make_begin_syntax(Exps) when is_list(Exps) ->
    ['begin-syntax'|Exps].

make_booleanp(Obj) ->
    ['boolean?', Obj].

make_booleansp(Objs) when is_list(Objs) ->
    ['boolean=?'|Objs].

make_bytevector_length(V) ->
    ['bytevector-length', V].

make_bytevector_ref(V) ->
    ['bytevector-ref', V].

make_bytevectorsp(Objs) when is_list(Objs) ->
    ['bytevector=?'|Objs].

make_call(Proc) ->
    make_call(Proc, []).

make_call(Proc, Args) when is_list(Args) ->
    [Proc|Args].

make_call_with_values(Producer, Consumer) ->
    ['call-with-values', Producer, Consumer].

make_callcc(Proc) ->
    ['call/cc', Proc].

make_car(Obj) ->
    ['car', Obj].

make_cdr(Obj) ->
    ['cdr', Obj].

make_caar(Obj) ->
    ['caar', Obj].

make_cadr(Obj) ->
    ['cadr', Obj].

make_cdar(Obj) ->
    ['cdar', Obj].

make_cddr(Obj) ->
    ['cddr', Obj].

make_case(Exp, Exps) when is_list(Exps) ->
    ['case'|[Exp|Exps]].

make_case(Exp, Exps, Else, false) when is_list(Exps), is_list(Else) ->
    ['case'|[Exp|Exps ++ [['else'|Else]]]];
make_case(Exp, Exps, Else, true) when is_list(Exps) ->
    ['case'|[Exp|Exps ++ [['else', '=>', Else]]]].

make_charsp(Objs) when is_list(Objs) ->
    ['char=?'|Objs].

make_char_to_integer(S) ->
    ['char->integer', S].

make_integer_to_char(S) ->
    ['integer->char', S].

make_cond(Exps) when is_list(Exps) ->
    ['cond'|Exps].

make_cond(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond'|Exps ++ [['else'|Else]]].

make_cond_expand(Exps) when is_list(Exps) ->
    ['cond-expand'|Exps].

make_cond_expand(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond-expand'|Exps ++ [['else'|Else]]].

make_cons(Obj1, Obj2) ->
    [Obj1|Obj2].

make_define(Formals, Exp) ->
    ['define', Formals, Exp].

make_define_values(Formals, Exp) ->
    ['define-values', Formals, Exp].

make_do(Test, Results) when is_list(Results) ->
    make_do(Test, Results, []).

make_do(Test, Results, Specs) when is_list(Results), is_list(Specs) ->
    make_do(Test, Results, Specs, []).

make_do(Test, Results, Specs, Commands) when is_list(Results), is_list(Specs), is_list(Commands) ->
    ['do', Specs, [Test|Results]] ++ Commands.

make_dynamic_wind(Before, Thunk, After) ->
    ['dynamic-wind', Before, Thunk, After].

make_eqvp(Obj1, Obj2) ->
    ['eqv?', Obj1, Obj2].

make_eqp(Obj1, Obj2) ->
    ['eq?', Obj1, Obj2].

make_equalp(Obj1, Obj2) ->
    ['equal?', Obj1, Obj2].

make_error(Message) ->
    make_error(Message, []).

make_error(Message, Objs) when is_list(Objs) ->
    ['error'|[Message|Objs]].

make_else(Exps) when is_list(Exps) ->
    ['else'|Exps].

make_foreach(Proc, Lists) when is_list(Lists) ->
    ['for-each', Proc] ++ Lists.

make_if(Test, Consequent) ->
    ['if', Test, Consequent].

make_if(Test, Consequent, Alternate) ->
    ['if', Test, Consequent, Alternate].

make_guard(Variable, Exps, Body) when is_list(Exps), is_list(Body) ->
    ['guard', [Variable|Exps]] ++ Body.

make_guard(Variable, Exps, Else, Body) when is_list(Exps), is_list(Else), is_list(Body) ->
    ['guard', [Variable|Exps ++ [['else'|Else]]]] ++ Body.

make_lambda(Formals, Body) when is_list(Body) ->
    ['lambda'|[Formals|Body]].

make_lambda_syntax(Formals, Body) when is_list(Body) ->
    ['lambda-syntax'|[Formals|Body]].

make_let(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let'|[Bindings|Body]].

make_let_named(Tag, Bindings, Body) when not is_list(Tag), is_list(Bindings), is_list(Body) ->
    ['let'|[Tag|[Bindings|Body]]].

make_lets(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let*'|[Bindings|Body]].

make_letrec(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['letrec'|[Bindings|Body]].

make_letrecs(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['letrec*'|[Bindings|Body]].

make_let_values(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let-values'|[Bindings|Body]].

make_lets_values(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let*-values'|[Bindings|Body]].

make_letrec_values(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['letrec-values'|[Bindings|Body]].

make_let_syntax(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let-syntax'|[Bindings|Body]].

make_lets_syntax(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['let*-syntax'|[Bindings|Body]].

make_letrec_syntax(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['letrec-syntax'|[Bindings|Body]].

make_letrecs_syntax(Bindings, Body) when is_list(Bindings), is_list(Body) ->
    ['letrec*-syntax'|[Bindings|Body]].

make_list(List) when is_list(List) ->
    ['list'|List].

make_map(Proc, Lists) when is_list(Lists) ->
    ['map', Proc] ++ Lists.

make_member(Obj, List) when is_list(List) ->
    ['member', Obj, List].

make_member(Obj, List, Compare) when is_list(List) ->
    ['member', Obj, List, Compare].

make_not(Exp) ->
    ['not', Exp].

make_nullp(Obj) ->
    ['null?', Obj].

make_or(Exps) when is_list(Exps) ->
    ['or'|Exps].

make_parameter(Init) ->
    ['make-parameter', Init].

make_parameter(Init, Converter) ->
    ['make-parameter', Init, Converter].

make_parameterize(Params, Body) when is_list(Params), is_list(Body) ->
    ['parameterize'|[Params|Body]].

make_quote(Exp) ->
    ['quote', Exp].

make_raise(Obj) ->
    ['raise', Obj].

make_raise_continuable(Obj) ->
    ['raise-continuable', Obj].

make_setb(Variable, Value) ->
    ['set!', Variable, Value].

make_setb_syntax(Variable, Value) ->
    ['set!-syntax', Variable, Value].

make_string_length(S) ->
    ['string-length', S].

make_string_ref(S) ->
    ['string-ref', S].

make_stringsp(Objs) when is_list(Objs) ->
    ['string=?'|Objs].

make_string_to_list(S) ->
    ['string->list', S].

make_string_to_list(S, Start) ->
    ['string->list', S, Start].

make_string_to_list(S, Start, End) ->
    ['string->list', S, Start, End].

make_list_to_string(L) ->
    ['list->string', L].

make_string_to_utf8(S) ->
    ['string->utf8', S].

make_string_to_utf8(S, Start) ->
    ['string->utf8', S, Start].

make_string_to_utf8(S, Start, End) ->
    ['string->utf8', S, Start, End].

make_utf8_to_string(V) ->
    ['utf8->string', V].

make_utf8_to_string(V, Start) ->
    ['utf8->string', V, Start].

make_utf8_to_string(V, Start, End) ->
    ['utf8->string', V, Start, End].

make_string_foreach(Proc, Lists) when is_list(Lists) ->
    ['string-for-each', Proc] ++ Lists.

make_string_map(Proc, Lists) when is_list(Lists) ->
    ['string-map', Proc] ++ Lists.

make_symbolsp(Objs) when is_list(Objs) ->
    ['symbol=?'|Objs].

make_symbol_to_string(S) ->
    ['symbol->string', S].

make_string_to_symbol(S) ->
    ['string->symbol', S].

make_thunk(Body) when is_list(Body) ->
    make_lambda([], Body).

make_unless(Test, Exps) when is_list(Exps) ->
    ['unless'|[Test|Exps]].

make_values(Args) when is_list(Args) ->
    ['values'|Args].

make_variable() ->
    erlang:make_ref().

make_variable(V) ->
    make_variable(make_variable(), V).

make_variable(V1, V2) when is_reference(V1) ->
    {V1, V2}.

make_vector_length(V) ->
    ['vector-length', V].

make_vector_ref(V) ->
    ['vector-ref', V].

make_vectorsp(Objs) when is_list(Objs) ->
    ['vector=?'|Objs].

make_vector_to_list(V) ->
    ['vector->list', V].

make_vector_to_list(V, Start) ->
    ['vector->list', V, Start].

make_vector_to_list(V, Start, End) ->
    ['vector->list', V, Start, End].

make_list_to_vector(L) ->
    ['list->vector', L].

make_vector_to_string(V) ->
    ['vector->string', V].

make_vector_to_string(V, Start) ->
    ['vector->string', V, Start].

make_vector_to_string(V, Start, End) ->
    ['vector->string', V, Start, End].

make_string_to_vector(S) ->
    ['string->vector', S].

make_vector_foreach(Proc, Lists) when is_list(Lists) ->
    ['vector-for-each', Proc] ++ Lists.

make_vector_map(Proc, Lists) when is_list(Lists) ->
    ['vector-map', Proc] ++ Lists.

make_when(Test, Exps) when is_list(Exps) ->
    ['when'|[Test|Exps]].

make_with_exception_handler(Handler, Thunk) ->
    ['with-exception-handler', Handler, Thunk].

-endif. % -ifndef(scmi_analyze).
