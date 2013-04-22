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

%%% @doc Scheme number tokenizer (base 16)
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

Definitions.

%% token - see LALR-1 parser generator

A = [aA]
%%B = [bB]
%%C = [cC]
%%D = [dD]
E = [eE]
F = [fF]
%%G = [gG]
%%H = [hH]
I = [iI]
%%J = [jJ]
%%K = [kK]
%%L = [lL]
%%M = [mM]
N = [nN]
%%O = [oO]
%%P = [pP]
%%Q = [qQ]
%%R = [rR]
%%S = [sS]
%%T = [tT]
%%U = [uU]
%%V = [vV]
%%W = [wW]
X = [xX]
%%Y = [yY]
%%Z = [zZ]

%% number - see LALR-1 parser generator

radixR            = #{X}

digitR            = [0-9a-fA-F]

uinfnan           = ({I}{N}{F}\.0|{N}{A}{N}\.0)

sign              = [+-]

exactness         = (#{I}|#{E})

rationaldigitsR   = {digitR}+/{digitR}+

digitsR           = {digitR}+

Rules.

%% number
{uinfnan}         : {token, {uinfnan, TokenLine, to_lower(TokenChars)}}.

{exactness}       : {token, {exactness, TokenLine, to_lower_atom(substr(TokenChars, 2))}}.

{radixR}          : {token, {radixR, TokenLine, 16}}.

{floatdigitsR}    : {token, {floatdigitsR, TokenLine, TokenChars}}.

{rationaldigitsR} : {token, {rationaldigitsR, TokenLine, TokenChars}}.

{digitsR}         : {token, {digitsR, TokenLine, TokenChars}}.

@                 : {token, {'@', TokenLine}}.

{I}               : {token, {'i', TokenLine}}.

{sign}            : {token, {sign, TokenLine, TokenChars}}.

Erlang code.

%%%----------------------------------------------------------------------
%%% Leex functions
%%%----------------------------------------------------------------------

substr(X, Start) ->
    string:substr(X, Start).

to_atom(X) ->
    list_to_atom(X).

to_lower(X) ->
    string:to_lower(X).

to_lower_atom(X) ->
    to_atom(to_lower(X)).
