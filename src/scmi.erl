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
-export([make_call/2
         , make_define/2
         , make_if/3
         , make_lambda/2
         , make_set/2
         , make_variable/0
        ]).

%% External types
-export_type([exec/0
              , env/0
              , ccok/0
              , ccng/0]).

-export_type([arg/0
              , vargs/0
              , val/0
              , f0/0
              , f/0
              , nip0/0
              , nipn/0
              , nipv/0
              , nipnv/0
              , nip/0
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

-type ana()    :: scmi_analyze:ana().
-type exec()   :: fun((env(), ccok(), ccng()) -> val()).
-type env()    :: scmi_env:env().
-type ccok()   :: fun((val(), ccng()) -> val()).
-type ccng()   :: fun((val()) -> no_return()).

-type arg()    :: scm_any().
-type vargs()  :: [arg()].
-type val()    :: scm_any().

-type param()  :: scm_symbol().
-type params() :: [param()].
-type body()   :: exec().

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

make_call(Proc, Args) ->
    [Proc|Args].

make_define(Variable, Value) ->
    ['define', Variable, Value].

make_if(Test, Consequent, Alternate) ->
    ['if', Test, Consequent, Alternate].

make_lambda(Formals, Body) ->
    ['lambda', Formals, Body].

make_set(Variable, Value) ->
    ['set!', Variable, Value].

make_variable() ->
    erlang:make_ref().

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
