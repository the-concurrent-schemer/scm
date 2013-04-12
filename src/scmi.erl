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

-module(scmi).

-include("scmd_type.hrl").
-include("scmi.hrl").

%% External exports
-export([make_and/1
         , make_begin/1
         , make_call/2
         , make_case/2, make_case/4
         , make_cond/1, make_cond/2
         , make_cond_expand/1, make_cond_expand/2
         , make_define/2
         , make_do/2, make_do/3, make_do/4
         , make_if/2, make_if/3
         , make_lambda/2
         , make_let/2
         , make_let_named/3
         , make_lets/2
         , make_letrec/2
         , make_letrecs/2
         , make_not/1
         , make_or/1
         , make_quote/1
         , make_set/2
         , make_unless/2
         , make_variable/0
         , make_when/2
        ]).

%% External types
-export_type([ana/0
              , exec/0
              , env/0
              , ccok/0
              , ccng/0]).

-export_type([arg/0
              , vargs/0
              , var/0
              , val/0
              , f0/0
              , f/0
              , nip0/0
              , nipn/0
              , nipv/0
              , nipnv/0
              , nip/0
              , xf0/0
              , xf/0
              , xnip0/0
              , xnipn/0
              , xnipv/0
              , xnipnv/0
              , xnip/0
              , p0/0
              , p/0
              , proc0/0
              , procn/0
              , procv/0
              , procnv/0
              , proc/0
             ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% context
-type ana()    :: scmi_analyze:ana().
-type exec()   :: fun((env(), ccok(), ccng()) -> val()).
-type env()    :: scmi_env:env().
-type ccok()   :: fun((val(), ccng()) -> val()).
-type ccng()   :: fun((val()) -> no_return()).

%% arg and vargs
-type arg()    :: scm_any().
-type vargs()  :: [arg()].

%% var and val
-type var()    :: scm_symbol() | reference().
-type val()    :: scm_any().

%% param, params, and body
-type param()  :: var().
-type params() :: [param()].
-type body()   :: exec().

%% native implemented procedures
-type f0()     :: fun(() -> val()).
-type fn()     :: fun((...) -> val()).      % fun((arg(),...) -> val()).
-type fv()     :: fun((vargs()) -> val()).
-type fnv()    :: fun((...) -> val()).      % fun((arg(),...,vargs()) -> val()).
-type f()      :: f0() | fn() | fv() | fnv().

-type nip0()   :: #nip0{val :: f0()}.
-type nipn()   :: #nipn{val :: fn() | [fn()]}.
-type nipv()   :: #nipv{val :: fv()}.
-type nipnv()  :: #nipnv{val :: fnv()}.
-type nip()    :: nip0() | nipn() | nipv() | nipnv().

%% extended-API native implemented procedures
-type xf0()    :: fun((scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).
-type xfn()    :: fun((...) -> val()).      % fun((arg(),... ,scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).
-type xfv()    :: fun((vargs(), scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).
-type xfnv()   :: fun((...) -> val()).      % fun((arg(),... ,vargs(), scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).
-type xf()     :: xf0() | xfn() | xfv() | xfnv().

-type xnip0()  :: #xnip0{val :: f0()}.
-type xnipn()  :: #xnipn{val :: fn() | [fn()]}.
-type xnipv()  :: #xnipv{val :: fv()}.
-type xnipnv() :: #xnipnv{val :: fnv()}.
-type xnip()   :: xnip0() | xnipn() | xnipv() | xnipnv().

%% lambda implemented procedures
-type p0()     :: {body(), env()}.
-type pn()     :: {params(), body(), env()}.
-type pv()     :: {param(), body(), env()}.
-type pnv()    :: {pos_integer(), params(), body(), env()}.
-type p()      :: p0() | pn() | pv() | pnv().

-type proc0()  :: #proc0{val :: p0()}.
-type procn()  :: #procn{val :: pn() | [pn()]}.
-type procv()  :: #procv{val :: pv()}.
-type procnv() :: #procnv{val :: pnv()}.
-type proc()   :: proc0() | procn() | procv() | procnv().

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

make_and(Exps) when is_list(Exps) ->
    ['and'|Exps].

make_begin(Exps) when is_list(Exps) ->
    ['begin'|Exps].

make_call(Proc, Args) when is_list(Args) ->
    [Proc|Args].

make_case(Exp, Exps) when is_list(Exps) ->
    ['case'|[Exp|Exps]].

make_case(Exp, Exps, Else, false) when is_list(Exps), is_list(Else) ->
    ['case'|[Exp|lists:append(Exps, [['else'|Else]])]];
make_case(Exp, Exps, Else, true) when is_list(Exps) ->
    ['case'|[Exp|lists:append(Exps, [['else', '=>', Else]])]].

make_cond(Exps) when is_list(Exps) ->
    ['cond'|Exps].

make_cond(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond'|lists:append(Exps, [['else'|Else]])].

make_cond_expand(Exps) when is_list(Exps) ->
    ['cond-expand'|Exps].

make_cond_expand(Exps, Else) when is_list(Exps), is_list(Else) ->
    ['cond-expand'|lists:append(Exps, [['else'|Else]])].

make_define(Variable, Value) ->
    ['define', Variable, Value].

make_do(Test, Results) when is_list(Results) ->
    make_do(Test, Results, []).

make_do(Test, Results, Specs) when is_list(Results), is_list(Specs) ->
    make_do(Test, Results, Specs, []).

make_do(Test, Results, Specs, Commands) when is_list(Results), is_list(Specs), is_list(Commands) ->
    lists:append(['do', Specs, [Test|Results]], Commands).

make_if(Test, Consequent) ->
    ['if', Test, Consequent].

make_if(Test, Consequent, Alternate) ->
    ['if', Test, Consequent, Alternate].

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

make_not(Exp) ->
    ['not', Exp].

make_or(Exps) when is_list(Exps) ->
    ['or'|Exps].

make_quote(Exps) when is_list(Exps) ->
    ['quote'|Exps];
make_quote(Exp) ->
    ['quote', Exp].

make_set(Variable, Value) ->
    ['set!', Variable, Value].

make_unless(Test, Exps) when is_list(Exps) ->
    ['unless'|[Test|Exps]].

make_variable() ->
    erlang:make_ref().

make_when(Test, Exps) when is_list(Exps) ->
    ['when'|[Test|Exps]].

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
