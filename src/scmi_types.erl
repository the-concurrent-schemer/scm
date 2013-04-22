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

%%% @doc Scheme interpreter types (for Erlang types and specs)
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_types).

-export_type([ana/0
              , exec/0
              , env/0
              , ccok/0
              , ccng/0]).

-export_type([arg/0
              , vargs/0
              , var/0
              , val/0
              , thunk/0
              , proc/0
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
              , l0/0
              , l/0
              , lip0/0
              , lipn/0
              , lipv/0
              , lipnv/0
              , lip/0
              , exception/0
              , error/0
             ]).

-include("scmd_types.hrl").

%% context
-type ana()         :: scmi_analyze:ana().
-type exec()        :: fun((env(), ccok(), ccng()) -> val()).
-type env()         :: scmi_env:env().
-type ccok()        :: fun((val(), ccng()) -> val()).
-type ccng()        :: fun((val()) -> no_return()).

%% arg and vargs
-type arg()         :: scm_any().
-type vargs()       :: [arg()].

%% var and val
-type var()         :: scm_symbol() | reference().
-type val()         :: scm_any().

%% param, params, and body
-type param()       :: var().
-type params()      :: [param()].
-type body()        :: exec().

%% thunk and proc
-type thunk()       :: nip0() | xnip0() | lip0().
-type proc()        :: nip() | xnip() | lip().

%% native implemented procedures
-type f0()          :: fun(() -> val()).
-type fn()          :: fun((...) -> val()).      % fun((arg(),...) -> val()).
-type fv()          :: fun((vargs()) -> val()).
-type fnv()         :: fun((...) -> val()).      % fun((arg(),...,vargs()) -> val()).
-type f()           :: f0() | fn() | fv() | fnv().

-type nip0()        :: #nip0{val :: f0()}.
-type nipn()        :: #nipn{val :: fn() | [fn()]}.
-type nipv()        :: #nipv{val :: fv()}.
-type nipnv()       :: #nipnv{val :: fnv()}.
-type nip()         :: nip0() | nipn() | nipv() | nipnv().

%% extended-API native implemented procedures
-type xf0()         :: fun((env(), ccok(), ccng()) -> val()).
-type xfn()         :: fun((...) -> val()).      % fun((arg(),... ,env(), ccok(), ccng()) -> val()).
-type xfv()         :: fun((vargs(), env(), ccok(), ccng()) -> val()).
-type xfnv()        :: fun((...) -> val()).      % fun((arg(),... ,vargs(), env(), ccok(), ccng()) -> val()).
-type xf()          :: xf0() | xfn() | xfv() | xfnv().

-type xnip0()       :: #xnip0{val :: xf0()}.
-type xnipn()       :: #xnipn{val :: xfn() | [xfn()]}.
-type xnipv()       :: #xnipv{val :: xfv()}.
-type xnipnv()      :: #xnipnv{val :: xfnv()}.
-type xnip()        :: xnip0() | xnipn() | xnipv() | xnipnv().

%% lambda implemented procedures
-type l0()          :: {body(), env()}.
-type ln()          :: {params(), body(), env()}.
-type lv()          :: {param(), body(), env()}.
-type lnv()         :: {pos_integer(), params(), body(), env()}.
-type l()           :: l0() | ln() | lv() | lnv().

-type lip0()        :: #lip0{val :: l0()}.
-type lipn()        :: #lipn{val :: ln() | [ln()]}.
-type lipv()        :: #lipv{val :: lv()}.
-type lipnv()       :: #lipnv{val :: lnv()}.
-type lip()         :: lip0() | lipn() | lipv() | lipnv().

%% exception and error
-type signal()      :: #signal{obj :: scm_any(), env :: env(), ccok :: ccok(), ccng :: ccng()}.
-type exception()   :: #exception{val :: [signal()]} | #cexception{val :: [signal()]}.

-type error_user()  :: #error_user{val :: [scm_any(),...]}.
-type error_read()  :: #error_read{val :: scm_any()}.
-type error_file()  :: #error_file{val :: scm_any()}.
-type error()       :: error_user() | error_read() | error_file().
