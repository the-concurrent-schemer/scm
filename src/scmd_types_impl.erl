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

%%% @doc Scheme datum implementation types (for Erlang types and specs)
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmd_types_impl).

-export_type([lineno/0, unichar/0, utf8/0]).

-export_type([s_complex/0
              , s_rectangular/0
              , s_polar/0
              , s_real/0
              , s_infnan/0
              , s_finite/0
              , s_finite_inexact/0
              , s_negzero/0
              , s_finite_exact/0
              , s_rational/0
              , s_numerator/0
              , s_denominator/0
              , s_integer/0
              , s_integer_inexact/0
              , s_integer_exact/0
              , s_integer_exact_neg/0
              , s_integer_exact_non_neg/0
              , s_integer_exact_non_zero/0
              , s_integer_exact_pos/0
             ]).

-export_type([s_boolean/0
              , s_bytevector/0
              , s_character/0
              , s_false/0
              , s_identifier/0
              , s_label/1
              , s_labelref/0
              , s_letter/0
              , s_list/1
              , s_list_nonempty/1
              , s_number/0
              , s_pair/1
              , s_string/0
              , s_symbol/0
              , s_true/0
              , s_vector/1
             ]).

-export_type([s_datum/0, s_any/1]).

-include("scmd_types_impl.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% helper
-type lineno()                   :: non_neg_integer().
-type integer_non_zero()         :: neg_integer() | pos_integer().

-type bytes()                    :: <<_:_ * 8>>.                      % 0..255
-type tuple(T)                   :: {T}.
-type unichar()                  :: unichar_low() | unichar_high().   % unicode "code point"
-type unichar_high()             :: 16#E000..16#10FFFF.
-type unichar_low()              :: 16#0000..16#D7FF.
-type utf8()                     :: <<_:_ * 8>>.                      % utf-8

-type t_label(T)                 :: {non_neg_integer(), T}.
-type t_list(T)                  :: maybe_improper_list(T, T | []).
-type t_list_nonempty(T)         :: nonempty_maybe_improper_list(T, T | []).
-type t_pair(T)                  :: nonempty_maybe_improper_list(T, T).
-type t_vector(T)                :: tuple(T).                         % tuple of any size

-type t_boolean()                :: boolean().
-type t_bytevector()             :: bytes().
-type t_character()              :: unichar().
-type t_false()                  :: false.
-type t_identifier()             :: atom() | {atom(), utf8()} | #mid{val :: t_symbol()}.   % atom(utf-8()) | {atom(sha(utf-8())), utf-8()} | #mid{}
-type t_labelref()               :: non_neg_integer().
-type t_letter()                 :: 65..90 | 97..122.
-type t_string()                 :: tuple(unichar()).
-type t_symbol()                 :: t_identifier() | t_variable().
-type t_true()                   :: true.
-type t_variable()               :: reference() | {reference(), t_symbol()}.

%% number
-type s_complex()                :: s_rectangular() | s_polar() | s_real().
-type s_rectangular()            :: {rectangular, {Real :: s_real(), Imag :: s_real()}}.
-type s_polar()                  :: {polar, {Mag :: s_real(), Ang :: s_rational()}}.
-type s_real()                   :: s_infnan() | s_finite().
-type s_infnan()                 :: ?PINF | ?NINF | ?PNAN | ?NNAN.
-type s_finite()                 :: s_finite_inexact() | s_finite_exact().
-type s_finite_inexact()         :: s_negzero() | float() | s_integer_inexact().
-type s_negzero()                :: ?NZER.
-type s_finite_exact()           :: s_rational() | s_integer_exact().
-type s_rational()               :: {s_numerator(), s_denominator()}.
-type s_numerator()              :: s_integer().
-type s_denominator()            :: s_integer_exact_pos().
-type s_integer()                :: s_integer_inexact() | s_integer_exact().
-type s_integer_inexact()        :: float().                          % trunc(float()) == float()
-type s_integer_exact()          :: integer().
-type s_integer_exact_neg()      :: neg_integer().
-type s_integer_exact_non_neg()  :: non_neg_integer().
-type s_integer_exact_non_zero() :: integer_non_zero().
-type s_integer_exact_pos()      :: pos_integer().

%% datum
-type s_boolean()                :: #boolean{val :: t_boolean()}.
-type s_bytevector()             :: #bytevector{val :: t_bytevector()}.
-type s_character()              :: #character{val :: t_character()}.
-type s_false()                  :: #boolean{val :: t_false()}.
-type s_identifier()             :: t_identifier().
-type s_label(T)                 :: #label{val :: t_label(T)}.
-type s_labelref()               :: #labelref{val :: t_labelref()}.
-type s_letter()                 :: #character{val :: t_letter()}.
-type s_list(T)                  :: t_list(T).
-type s_list_nonempty(T)         :: t_list_nonempty(T).
-type s_number()                 :: s_complex().
-type s_pair(T)                  :: t_pair(T).
-type s_string()                 :: #string{val :: t_string()}.
-type s_symbol()                 :: t_symbol().
-type s_true()                   :: #boolean{val :: t_true()}.
-type s_vector(T)                :: #vector{val :: t_vector(T)}.

-type s_datum()                  :: s_any(s_datum()).

-type s_any(T)                   :: s_boolean() |
                                    s_number() |
                                    s_character() |
                                    s_string() |
                                    s_symbol() |
                                    s_bytevector() |
                                    s_list(T) |
                                    s_list_nonempty(T) |
                                    s_pair(T) |
                                    s_vector(T) |
                                    s_label(T) |
                                    s_labelref().
