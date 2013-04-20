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

-ifndef(scmi_analyze).
-define(scmi_analyze, true).

-include("scmi.hrl").

-record(ana, {
          file    :: scmd_parse:filename(),  % Current file
          path=[] :: [scmd_parse:filename()] % Include-path
         }).

%% Default imports
-compile({nowarn_unused_function,
          [make_apply/2
           , make_and/1
           , make_begin/1
           , make_call/1, make_call/2
           , make_call_with_values/2
           , make_callcc/1
           , make_car/1, make_cdr/1, make_caar/1, make_cadr/1, make_cdar/1, make_cddr/1
           , make_case/2, make_case/4
           , make_cond/1, make_cond/2
           , make_cond_expand/1, make_cond_expand/2
           , make_define/2
           , make_define_values/2
           , make_do/2, make_do/3, make_do/4
           , make_dynamic_wind/3
           , make_eqp/2, make_eqvp/2
           , make_error/1, make_error/2
           , make_else/1
           , make_if/2, make_if/3
           , make_guard/3, make_guard/4
           , make_lambda/2
           , make_let/2
           , make_let_named/3
           , make_lets/2
           , make_letrec/2
           , make_letrecs/2
           , make_let_values/2
           , make_lets_values/2
           , make_letrec_values/2
           , make_not/1
           , make_nullp/1
           , make_or/1
           , make_parameter/1, make_parameter/2
           , make_parameterize/2
           , make_quote/1
           , make_raise/1
           , make_raise_continuable/1
           , make_setb/2
           , make_thunk/1
           , make_unless/2
           , make_values/1
           , make_variable/0
           , make_when/2
           , make_with_exception_handler/2
          ]}).

%% @TBD
%% -compile({inline,
%%           [make_apply/2
%%            , make_and/1
%%            , make_begin/1
%%            , make_call/1, make_call/2
%%            , make_call_with_values/2
%%            , make_callcc/1
%%            , make_car/1, make_cdr/1, make_caar/1, make_cadr/1, make_cdar/1, make_cddr/1
%%            , make_case/2, make_case/4
%%            , make_cond/1, make_cond/2
%%            , make_cond_expand/1, make_cond_expand/2
%%            , make_define/2
%%            , make_define_values/2
%%            , make_do/2, make_do/3, make_do/4
%%            , make_dynamic_wind/3
%%            , make_eqp/2, make_eqvp/2
%%            , make_error/1, make_error/2
%%            , make_else/1
%%            , make_if/2, make_if/3
%%            , make_guard/3, make_guard/4
%%            , make_lambda/2
%%            , make_let/2
%%            , make_let_named/3
%%            , make_lets/2
%%            , make_letrec/2
%%            , make_letrecs/2
%%            , make_let_values/2
%%            , make_lets_values/2
%%            , make_letrec_values/2
%%            , make_not/1
%%            , make_nullp/1
%%            , make_or/1
%%            , make_parameter/1, make_parameter/2
%%            , make_parameterize/2
%%            , make_quote/1
%%            , make_raise/1
%%            , make_raise_continuable/1
%%            , make_setb/2
%%            , make_thunk/1
%%            , make_unless/2
%%            , make_values/1
%%            , make_variable/0
%%            , make_when/2
%%            , make_with_exception_handler/2
%%           ]}).

make_apply(Proc, Args) when is_list(Args) ->
    ['apply'|[Proc|Args]].

make_and(Exps) when is_list(Exps) ->
    ['and'|Exps].

make_begin(Exps) when is_list(Exps) ->
    ['begin'|Exps].

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

make_cond(Exps) when is_list(Exps) ->
    ['cond'|Exps].

make_cond(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond'|Exps ++ [['else'|Else]]].

make_cond_expand(Exps) when is_list(Exps) ->
    ['cond-expand'|Exps].

make_cond_expand(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond-expand'|Exps ++ [['else'|Else]]].

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

make_eqp(Obj1, Obj2) ->
    ['eq?', Obj1, Obj2].

make_eqvp(Obj1, Obj2) ->
    ['eqv?', Obj1, Obj2].

make_error(Message) ->
    make_error(Message, []).

make_error(Message, Objs) when is_list(Objs) ->
    ['error'|[Message|Objs]].

make_else(Exps) when is_list(Exps) ->
    ['else'|Exps].

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

make_quote(Exps) when is_list(Exps) ->
    ['quote'|Exps];
make_quote(Exp) ->
    ['quote', Exp].

make_raise(Obj) ->
    ['raise', Obj].

make_raise_continuable(Obj) ->
    ['raise-continuable', Obj].

make_setb(Variable, Value) ->
    ['set!', Variable, Value].

make_thunk(Body) when is_list(Body) ->
    make_lambda([], Body).

make_unless(Test, Exps) when is_list(Exps) ->
    ['unless'|[Test|Exps]].

make_values(Args) when is_list(Args) ->
    ['values'|Args].

make_variable() ->
    erlang:make_ref().

make_when(Test, Exps) when is_list(Exps) ->
    ['when'|[Test|Exps]].

make_with_exception_handler(Handler, Thunk) ->
    ['with-exception-handler', Handler, Thunk].

-endif. % -ifndef(scmi_analyze).
