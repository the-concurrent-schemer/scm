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

-ifndef(scmi).
-define(scmi, true).

-include("scmd_type.hrl").

-type scmi_vargs()  :: scmi:vargs().

-type scmi_f0()     :: scmi:f0().
-type scmi_f()      :: scmi:f().

-type scmi_nip0()   :: scmi:nip0().
-type scmi_nipn()   :: scmi:nipn().
-type scmi_nipv()   :: scmi:nipv().
-type scmi_nipnv()  :: scmi:nipnv().
-type scmi_nip()    :: scmi:nip().

-type scmi_p0()     :: scmi:p0().
-type scmi_p()      :: scmi:p().

-type scmi_proc0()  :: scmi:proc0().
-type scmi_procn()  :: scmi:procn().
-type scmi_procv()  :: scmi:procv().
-type scmi_procnv() :: scmi:procnv().
-type scmi_proc()   :: scmi:proc().

-type scmi_ana()    :: scmi:ana().
-type scmi_exec()   :: scmi:exec().
-type scmi_env()    :: scmi:env().
-type scmi_ccok()   :: scmi:ccok().
-type scmi_ccng()   :: scmi:ccng().

-endif. % -ifndef(scmi).
