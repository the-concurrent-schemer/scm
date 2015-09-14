%%% -*- mode: erlang -*-
%%%
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
%%%

-ifndef(scmd_parse_numR).
-define(scmd_parse_numR, true).

-record(n,   {sign :: sign(),  val  :: token()}).                         % real integer
-record(q,   {sign :: sign(),  num  :: token(), den :: token()}).         % real rational
-record(x,   {sign :: sign(),  val  :: token() | int(), exp :: token()}). % real float
-record(in,  {sign :: sign(),  val  :: token()}).                         % infnan
-record(zp,  {mag  :: real(),  ang  :: real()}).                          % complex polar
-record(zr,  {real :: real(),  imag :: real()}).                          % complex rectangular
-record(numR, {                                                           % number
          lineno    :: pos_integer(),
          exactness :: exactness(),
          radix     :: radix(),
          val       :: complex()
         }).

-type exactness()   :: e | i.
-type radix()       :: 2 | 8 | 10 | 16.
-type sign()        :: string().                                          % "+" | "-"
-type token()       :: string().
-type int()         :: #n{} | #q{}.
-type real()        :: int() | #x{} | #in{}.
-type complex()     :: real() | #zp{} | #zr{}.

-endif. % -ifndef(scmd_parse_numR).
