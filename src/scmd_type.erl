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

-module(scmd_type).

-include("scmd.hrl").

-export_type([scm_alist/0
              , scm_boolean/0, scm_true/0, scm_false/0
              , scm_byte/0
              , scm_bytevector/0
              , scm_char/0
              , scm_end/0
              , scm_k/0, scm_k_pos/0
              , scm_letter/0
              , scm_list/0, scm_list_nonempty/0
              , scm_n/0, scm_n_nonzero/0, scm_n_pos/0
              , scm_obj/0
              , scm_pair/0
              , scm_port/0, scm_eof/0, scm_readerr/0, scm_fileerr/0
              , scm_proc/0
              , scm_q/0
              , scm_start/0
              , scm_string/0
              , scm_symbol/0
              , scm_thunk/0
              , scm_vector/0
              , scm_x/0
              , scm_y/0
              , scm_z/0
             ]).

-type scm_alist()           :: [scm_pair()].                         % association list (list of pairs)
-type scm_boolean()         :: scmd:s_boolean().                     % boolean value (#t or #f)
-type scm_true()            :: scmd:s_true().                        % true
-type scm_false()           :: scmd:s_false().                       % false
-type scm_byte()            :: scmd:s_byte().                        % exact integer 0 =< byte < 256
-type scm_bytevector()      :: scmd:s_bytevector().                  % bytevector
-type scm_char()            :: scmd:s_character().                   % character
-type scm_end()             :: scmd:s_integer_exact_non_neg().       % exact non-negative integer
-type scm_k()               :: scmd:s_integer_exact_non_neg().       % exact non-negative integer
-type scm_k_pos()           :: scmd:s_integer_exact_pos().           % exact positive integer
-type scm_letter()          :: scmd:s_letter().                      % alphabetic character
-type scm_list()            :: scmd:s_list(scm_any()).               % list
-type scm_list_nonempty()   :: scmd:s_list_nonempty(scm_any()).      % non-empty list
-type scm_n()               :: scmd:s_integer().                     % integer
-type scm_n_nonzero()       :: scmd:s_integer_nonzero().             % non-zero integer
-type scm_n_pos()           :: scmd:s_integer_pos().                 % positive integer
-type scm_obj()             :: scm_any().                            % any object
-type scm_pair()            :: scmd:s_pair(scm_any()).               % pair
-type scm_port()            :: io:dev().                             % port
-type scm_eof()             :: 'eof'.                                % end-of-file
-type scm_readerr()         :: 'readerr'.                            % read error
-type scm_fileerr()         :: 'fileerr'.                            % file error
-type scm_proc()            :: scmi:f().                             % proc
-type scm_q()               :: scmd:s_rational().                    % rational
-type scm_start()           :: scmd:s_integer_exact_non_neg().       % exact non-negative integer
-type scm_string()          :: scmd:s_string().                      % string
-type scm_symbol()          :: scmd:s_symbol().                      % symbol
-type scm_thunk()           :: scmi:f0().                            % thunk
-type scm_vector()          :: scmd:s_vector(scm_any()).             % vector
-type scm_x()               :: scmd:s_real().                        % real number
-type scm_y()               :: scmd:s_real().                        % real number
-type scm_z()               :: scmd:s_complex().                     % complex number

-type scm_any()             :: scmd:s_any(scm_any())
                             | scm_alist()
                             | scm_boolean()
                             | scm_byte()
                             | scm_bytevector()
                             | scm_char()
                             | scm_end()
                             | scm_k()
                             | scm_letter()
                             | scm_list()
                             | scm_list_nonempty()
                             | scm_n()
                             | scm_n_nonzero()
                             | scm_n_pos()
                             | scm_pair()
                             | scm_port() | scm_eof() | scm_readerr() | scm_fileerr()
                             | scm_proc()
                             | scm_q()
                             | scm_start()
                             | scm_string()
                             | scm_symbol()
                             | scm_thunk()
                             | scm_vector()
                             | scm_x()
                             | scm_y()
                             | scm_z().
