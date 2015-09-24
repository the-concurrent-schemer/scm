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

%%% @doc Scheme interpreter types (for Erlang types and specs)
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_types).

-export_type([exp/0]).

-export_type([sexec/0
             , senv/0]).

-export_type([expander/0]).

-export_type([dexec/0
             , denv/0
             , dok/0
             , dng/0]).

-export_type([arg/0
             , vargs/0
             , var/0
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

-export_type([iodev/0
             , eof/0
             ]).

-include("scmi_types.hrl").

%% expression
-type exp()         :: scm_any().

%% analyze
-type sexec()       :: fun((exp(), senv()) -> expander() | dexec()).
-type senv()        :: scmi_analyze:senv().

%% expander
-type expander()    :: #expander{val :: sexec()}.

%% evaluate
-type dexec()       :: fun((denv(), dok(), dng()) -> exp()).
-type denv()        :: scmi_env:env().
-type dok()         :: fun((exp(), dng()) -> exp()).
-type dng()         :: fun((exp()) -> no_return()).

%% arg and vargs
-type arg()         :: exp().
-type vargs()       :: [arg()].

%% var
-type var()         :: scm_symbol() | reference().

%% param, params, body, and src
-type param()       :: var().
-type params()      :: [param()].
-type body()        :: dexec().
-type src()         :: fun(() -> [exp()]).

%% thunk and proc
-type thunk()       :: nip0() | xnip0() | lip0().
-type proc()        :: nip() | xnip() | lip().

%% native implemented procedures
-type f0()          :: fun(() -> exp()).
-type fn()          :: fun((...) -> exp()).      % fun((arg(),...) -> exp()).
-type fv()          :: fun((vargs()) -> exp()).
-type fnv()         :: fun((...) -> exp()).      % fun((arg(),...,vargs()) -> exp()).
-type f()           :: f0() | fn() | fv() | fnv().

-type nip0()        :: #nip0{val :: f0()}.
-type nipn()        :: #nipn{val :: fn() | [fn()]}.
-type nipv()        :: #nipv{val :: fv()}.
-type nipnv()       :: #nipnv{val :: fnv()}.
-type nip()         :: nip0() | nipn() | nipv() | nipnv().

%% extended-API native implemented procedures
-type xf0()         :: fun((denv(), dok(), dng()) -> exp()).
-type xfn()         :: fun((...) -> exp()).      % fun((arg(),... ,denv(), dok(), dng()) -> exp()).
-type xfv()         :: fun((vargs(), denv(), dok(), dng()) -> exp()).
-type xfnv()        :: fun((...) -> exp()).      % fun((arg(),... ,vargs(), denv(), dok(), dng()) -> exp()).
-type xf()          :: xf0() | xfn() | xfv() | xfnv().

-type xnip0()       :: #xnip0{val :: xf0()}.
-type xnipn()       :: #xnipn{val :: xfn() | [xfn()]}.
-type xnipv()       :: #xnipv{val :: xfv()}.
-type xnipnv()      :: #xnipnv{val :: xfnv()}.
-type xnip()        :: xnip0() | xnipn() | xnipv() | xnipnv().

%% lambda implemented procedures
-type l0()          :: #l0{body :: body(), env :: denv(), src :: src()}.
-type ln()          :: #ln{params :: params(), body :: body(), env :: denv(), src :: src()}.
-type lv()          :: #lv{param :: param(), body :: body(), env :: denv(), src :: src()}.
-type lnv()         :: #lnv{n :: pos_integer(), params :: params(), body :: body(), env :: denv(), src :: src()}.
-type l()           :: l0() | ln() | lv() | lnv().

-type lip0()        :: #lip0{val :: l0()}.
-type lipn()        :: #lipn{val :: ln() | [ln()]}.
-type lipv()        :: #lipv{val :: lv()}.
-type lipnv()       :: #lipnv{val :: lnv()}.
-type lip()         :: lip0() | lipn() | lipv() | lipnv().

%% exception and error
-type signal()      :: #signal{obj :: exp(), env :: denv(), ok :: dok(), ng :: dng()}.
-type exception()   :: #exception{val :: [signal()]} | #cexception{val :: [signal()]}.

-type error_user()  :: #error_user{val :: [exp(),...]}.
-type error_read()  :: #error_read{val :: exp()}.
-type error_file()  :: #error_file{val :: exp()}.
-type error()       :: error_user() | error_read() | error_file().

%% io devices
-type iodev()       :: scmi_iodev:iodev().
-type eof()         :: ?EOF. % ?EOF
