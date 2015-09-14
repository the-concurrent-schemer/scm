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

-ifndef(scmd_types).
-define(scmd_types, true).

-include("scmd_types_impl.hrl").

-type scm_alist()           :: scmd_types:scm_alist().
-type scm_boolean()         :: scmd_types:scm_boolean().
-type scm_true()            :: scmd_types:scm_true().
-type scm_false()           :: scmd_types:scm_false().
-type scm_byte()            :: scmd_types:scm_byte().
-type scm_bytevector()      :: scmd_types:scm_bytevector().
-type scm_char()            :: scmd_types:scm_char().
-type scm_end()             :: scmd_types:scm_end().
-type scm_k()               :: scmd_types:scm_k().
-type scm_k_pos()           :: scmd_types:scm_k_pos().
-type scm_letter()          :: scmd_types:scm_letter().
-type scm_list()            :: scmd_types:scm_list().
-type scm_list_nonempty()   :: scmd_types:scm_list_nonempty().
-type scm_n()               :: scmd_types:scm_n().
-type scm_n_nonzero()       :: scmd_types:scm_n_nonzero().
-type scm_n_pos()           :: scmd_types:scm_n_pos().
-type scm_obj()             :: scmd_types:scm_obj().
-type scm_pair()            :: scmd_types:scm_pair().
-type scm_port()            :: scmd_types:scm_port().
-type scm_eof()             :: scmd_types:scm_eof().
-type scm_proc()            :: scmd_types:scm_proc().
-type scm_q()               :: scmd_types:scm_q().
-type scm_start()           :: scmd_types:scm_start().
-type scm_string()          :: scmd_types:scm_string().
-type scm_symbol()          :: scmd_types:scm_symbol().
-type scm_thunk()           :: scmd_types:scm_thunk().
-type scm_vector()          :: scmd_types:scm_vector().
-type scm_x()               :: scmd_types:scm_x().
-type scm_y()               :: scmd_types:scm_y().
-type scm_z()               :: scmd_types:scm_z().

-type scm_exception()       :: scmd_types:scm_exception().
-type scm_error()           :: scmd_types:scm_error().

-type scm_any()             :: smc_type:scm_any().

-endif. % -ifndef(scmd_types).
