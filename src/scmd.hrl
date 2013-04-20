%%% -*- mode: erlang -*-
%%%
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
%%%
%%% @doc scheme datum

-ifndef(scmd).
-define(scmd, true).

-define(UNASSIGNED,       '').

-define(PINF,             {+1, 'inf.0'}).
-define(NINF,             {-1, 'inf.0'}).
-define(PNAN,             {+1, 'nan.0'}).
-define(NNAN,             {-1, 'nan.0'}).
-define(NZER,             {-1, '0.0'}).

-define(FALSE,            #boolean{val=false}).
-define(TRUE,             #boolean{val=true}).

-define(UNICHAR_LOW_MIN,  16#00).
-define(NULL,             16#00).
-define(ALARM,            16#07).
-define(BACKSPACE,        16#08).
-define(TAB,              16#09).
-define(NEWLINE,          16#0A).
-define(RETURN,           16#0D).
-define(ESCAPE,           16#1B).
-define(SPACE,            16#20).
-define(DQUOTE,           16#22).
-define(BACKSLASH,        16#5C).
-define(VLINE,            16#7C).
-define(DELETE,           16#7F).
-define(UNICHAR_LOW_MAX,  16#D7FF).
-define(UNICHAR_HIGH_MIN, 16#E000).
-define(UNICHAR_HIGH_MAX, 16#10FFFF).

-record(boolean,          {lineno :: scmd:lineno(), val}).
-record(bytevector,       {lineno :: scmd:lineno(), val}).
-record(character,        {lineno :: scmd:lineno(), val}).
-record(label,            {lineno :: scmd:lineno(), val}).
-record(labelref,         {lineno :: scmd:lineno(), val}).
-record(quasiquote,       {lineno :: scmd:lineno(), val}).
-record(quote,            {lineno :: scmd:lineno(), val}).
-record(string,           {lineno :: scmd:lineno(), val}).
-record(unquote,          {lineno :: scmd:lineno(), val}).
-record(unquote_splicing, {lineno :: scmd:lineno(), val}).
-record(vector,           {lineno :: scmd:lineno(), val}).

-record(nip0,             {lineno :: scmd:lineno(), val}).
-record(nipn,             {lineno :: scmd:lineno(), val}).
-record(nipv,             {lineno :: scmd:lineno(), val}).
-record(nipnv,            {lineno :: scmd:lineno(), val}).

-record(xnip0,            {lineno :: scmd:lineno(), val}).
-record(xnipn,            {lineno :: scmd:lineno(), val}).
-record(xnipv,            {lineno :: scmd:lineno(), val}).
-record(xnipnv,           {lineno :: scmd:lineno(), val}).

-record(lip0,             {lineno :: scmd:lineno(), val}).
-record(lipn,             {lineno :: scmd:lineno(), val}).
-record(lipv,             {lineno :: scmd:lineno(), val}).
-record(lipnv,            {lineno :: scmd:lineno(), val}).

-record(error_file,       {lineno :: scmd:lineno(), val}).
-record(error_read,       {lineno :: scmd:lineno(), val}).
-record(error_user,       {lineno :: scmd:lineno(), val}).

-record(exception,        {lineno :: scmd:lineno(), val}).
-record(cexception,       {lineno :: scmd:lineno(), val}).

-record(signal,           {obj, env, ccok, ccng}).

-endif. % -ifndef(scmd).
