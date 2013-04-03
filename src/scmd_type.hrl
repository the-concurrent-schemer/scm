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

-ifndef(scmd_type).
-define(scmd_type, true).

-include("scmd.hrl").

-type scm_alist()           :: scmd_type:scm_alist().
-type scm_boolean()         :: scmd_type:scm_boolean().
-type scm_true()            :: scmd_type:scm_true().
-type scm_false()           :: scmd_type:scm_false().
-type scm_byte()            :: scmd_type:scm_byte().
-type scm_bytevector()      :: scmd_type:scm_bytevector().
-type scm_char()            :: scmd_type:scm_char().
-type scm_end()             :: scmd_type:scm_end().
-type scm_k()               :: scmd_type:scm_k().
-type scm_k_pos()           :: scmd_type:scm_k_pos().
-type scm_letter()          :: scmd_type:scm_letter().
-type scm_list()            :: scmd_type:scm_list().
-type scm_list_nonempty()   :: scmd_type:scm_list_nonempty().
-type scm_n()               :: scmd_type:scm_n().
-type scm_n_nonzero()       :: scmd_type:scm_n_nonzero().
-type scm_n_pos()           :: scmd_type:scm_n_pos().
-type scm_obj()             :: scmd_type:scm_obj().
-type scm_pair()            :: scmd_type:scm_pair().
-type scm_port()            :: scmd_type:scm_port().
-type scm_eof()             :: scmd_type:scm_eof().
-type scm_proc()            :: scmd_type:scm_proc().
-type scm_q()               :: scmd_type:scm_q().
-type scm_start()           :: scmd_type:scm_start().
-type scm_string()          :: scmd_type:scm_string().
-type scm_symbol()          :: scmd_type:scm_symbol().
-type scm_thunk()           :: scmd_type:scm_thunk().
-type scm_vector()          :: scmd_type:scm_vector().
-type scm_x()               :: scmd_type:scm_x().
-type scm_y()               :: scmd_type:scm_y().
-type scm_z()               :: scmd_type:scm_z().

-type scm_any()             :: smc_type:scm_any().

-endif. % -ifndef(scmd_type).
