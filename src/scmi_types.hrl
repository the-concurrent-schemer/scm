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

-ifndef(scmi_types).
-define(scmi_types, true).

-include("scmd_types.hrl").

%% context
-type scmi_ana()       :: scmi_types:ana().
-type scmi_exec()      :: scmi_types:exec().
-type scmi_env()       :: scmi_types:env().
-type scmi_ccok()      :: scmi_types:ccok().
-type scmi_ccng()      :: scmi_types:ccng().

%% vargs
-type scmi_vargs()     :: scmi_types:vargs().

%% var
-type scmi_var()       :: scmi_types:var().

%% thunk and proc
-type scmi_thunk()     :: scmi_types:thunk().
-type scmi_proc()      :: scmi_types:proc().

%% native implemented procedures
-type scmi_f0()        :: scmi_types:f0().
-type scmi_f()         :: scmi_types:f().

-type scmi_nip0()      :: scmi_types:nip0().
-type scmi_nipn()      :: scmi_types:nipn().
-type scmi_nipv()      :: scmi_types:nipv().
-type scmi_nipnv()     :: scmi_types:nipnv().
-type scmi_nip()       :: scmi_types:nip().

%% extended-API native implemented procedures
-type scmi_xf0()       :: scmi_types:xf0().
-type scmi_xf()        :: scmi_types:xf().

-type scmi_xnip0()     :: scmi_types:xnip0().
-type scmi_xnipn()     :: scmi_types:xnipn().
-type scmi_xnipv()     :: scmi_types:xnipv().
-type scmi_xnipnv()    :: scmi_types:xnipnv().
-type scmi_xnip()      :: scmi_types:xnip().

%% lambda implemented procedures
-type scmi_l0()        :: scmi_types:l0().
-type scmi_l()         :: scmi_types:l().

-type scmi_lip0()      :: scmi_types:lip0().
-type scmi_lipn()      :: scmi_types:lipn().
-type scmi_lipv()      :: scmi_types:lipv().
-type scmi_lipnv()     :: scmi_types:lipnv().
-type scmi_lip()       :: scmi_types:lip().

-record(l0,            {body, env, src}).
-record(ln,            {params, body, env, src}).
-record(lv,            {param, body, env, src}).
-record(lnv,           {n, params, body, env, src}).

%% exception and error
-type scmi_exception() :: scmi_types:exception().
-type scmi_error()     :: scmi_types:error().

-record(signal,        {obj, env, ccok, ccng}).

%% io devices
-type scmi_iodev()     :: scmi_types:iodev().
-type scmi_eof()       :: scmi_types:eof().

-endif. % -ifndef(scmi_types).
