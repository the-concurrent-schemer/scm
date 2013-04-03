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
%%% @doc LALR-1 parser generator

Nonterminals numberR complexR polarR rectangularR realR srealR urealR
decimalR uintegerR prefixR.

Terminals '@' 'i' sign uinfnan suffix exactness radixR digitsR
rationaldigitsR floatdigitsR.

Rootsymbol numberR.

numberR -> prefixR complexR :
     #numR{lineno=line_of('$1'), exactness=value_of('$1'), val=value_of('$2')}.
numberR -> complexR :
     #numR{lineno=line_of('$1'), val=value_of('$1')}.

complexR -> polarR :
     '$1'.
complexR -> rectangularR :
     '$1'.
complexR -> realR :
     '$1'.

polarR -> realR '@' realR :
     token(polarR, line_of('$1'), #zp{mag=value_of('$1'), ang=value_of('$3')}).

rectangularR -> realR srealR 'i' :
     token(rectangularR, line_of('$1'), #zr{real=value_of('$1'), imag=value_of('$2')}).
rectangularR -> realR sign 'i' :
     token(rectangularR, line_of('$1'), #zr{real=value_of('$1'), imag=#n{sign=value_of('$2'), val="1"}}).
rectangularR -> srealR 'i' :
     token(rectangularR, line_of('$1'), #zr{real=#n{val="0"}, imag=value_of('$1')}).
rectangularR -> sign 'i' :
     token(rectangularR, line_of('$1'), #zr{real=#n{val="0"}, imag=#n{sign=value_of('$1'), val="1"}}).

realR -> srealR :
     '$1'.
realR -> urealR :
     '$1'.

srealR -> sign urealR :
     token(srealR, line_of('$1'), update_sign(value_of('$1'), value_of('$2'))).
srealR -> sign uinfnan :
     token(srealR, line_of('$1'), #in{sign=value_of('$1'), val=value_of('$2')}).

urealR -> rationaldigitsR :
     [Num, Den] = string:tokens(value_of('$1'), "/"),
           token(urealR, line_of('$1'), #q{num=Num, den=Den}).
urealR -> decimalR :
     '$1'.

decimalR -> floatdigitsR suffix :
     token(decimalR, line_of('$1'), #x{val=value_of('$1'), exp=value_of('$2')}).
decimalR -> uintegerR suffix :
     token(decimalR, line_of('$1'), #x{val=value_of('$1'), exp=value_of('$2')}).
decimalR -> floatdigitsR :
     token(decimalR, line_of('$1'), #x{val=value_of('$1')}).
decimalR -> uintegerR :
     '$1'.

uintegerR -> digitsR :
     token(uintegerR, line_of('$1'), #n{val=value_of('$1')}).

prefixR -> exactness radixR :
     token(prefixR, line_of('$1'), value_of('$1')).
prefixR -> radixR exactness :
     token(prefixR, line_of('$1'), value_of('$2')).
prefixR -> exactness :
     token(prefixR, line_of('$1'), value_of('$1')).
prefixR -> radixR :
     token(prefixR, line_of('$1'), undefined).

Erlang code.

-include("scmi.hrl").
-include("scmd_parse_numR.hrl").

-export([to_number/3]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec to_number(scmd:lineno(), string(), scmi_env()) -> {ok, scmd:e_number()} | {error, scmd:lineno(), Msg::iolist()}.
to_number(N, X, Env) ->
    case parse_number(N, X) of
        {ok, #numR{lineno=N1, exactness=E, radix=R, val=Y}} ->
            try
                {ok, to_number(E, R, Y, Env)}
            catch
                throw:{error, {N1, ?MODULE, Msg}} ->
                    {error, N1, Msg}
            end;
        {error, {N1, ?MODULE, Msg}} ->
            {error, N1, Msg}
    end.

%%%----------------------------------------------------------------------
%%% Yecc functions
%%%----------------------------------------------------------------------

token(Category, Line, Value) ->
    {Category, Line, Value}.

line_of(Token) ->
    element(2, Token).

value_of(Token) when tuple_size(Token) == 2 ->
    element(1, Token);
value_of(Token) when tuple_size(Token) == 3 ->
    element(3, Token).

update_sign(X, Y) when is_record(Y, n) ->
    Y#n{sign=X};
update_sign(X, Y) when is_record(Y, q) ->
    Y#q{sign=X};
update_sign(X, Y) when is_record(Y, x) ->
    Y#x{sign=X}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

parse_number(N, X) ->
    case case X of
             [$#, A | _]        when A==$b; A==$B ->
                 {2, scmd_parse:string(scmd_scan_num2,   ?MODULE, X, N)};
             [$#, A | _]        when A==$o; A==$O ->
                 {8, scmd_parse:string(scmd_scan_num8,   ?MODULE, X, N)};
             [$#, A | _]        when A==$d; A==$D ->
                 {10, scmd_parse:string(scmd_scan_num10, ?MODULE, X, N)};
             [$#, A | _]        when A==$x; A==$X ->
                 {16, scmd_parse:string(scmd_scan_num16, ?MODULE, X, N)};
             [$#, _, $#, B | _] when B==$b; B==$B ->
                 {2, scmd_parse:string(scmd_scan_num2,   ?MODULE, X, N)};
             [$#, _, $#, B | _] when B==$o; B==$O ->
                 {8, scmd_parse:string(scmd_scan_num8,   ?MODULE, X, N)};
             [$#, _, $#, B | _] when B==$d; B==$D ->
                 {10, scmd_parse:string(scmd_scan_num10, ?MODULE, X, N)};
             [$#, _, $#, B | _] when B==$x; B==$X ->
                 {16, scmd_parse:string(scmd_scan_num16, ?MODULE, X, N)};
             _ ->
                 {10, scmd_parse:string(scmd_scan_num10, ?MODULE, X, N)}
         end
    of
        {R, {ok, Y}} ->
            {ok, Y#numR{radix=R}};
        {_, Err} ->
            Err
    end.

to_number(E, R, X, Env) ->
    to_exactness(E, from_numR(R, X, Env), Env).

from_numR(R, X, Env) when is_record(X, n) ->
    from_nR(R, X, Env);
from_numR(R, X, Env) when is_record(X, q) ->
    from_qR(R, X, Env);
from_numR(R, X, Env) when is_record(X, x) ->
    from_xR(R, X, Env);
from_numR(R, X, Env) when is_record(X, in) ->
    from_in(R, X, Env);
from_numR(R, X, Env) when is_record(X, zp) ->
    from_zpR(R, X, Env);
from_numR(R, X, Env) when is_record(X, zr) ->
    from_zrR(R, X, Env).

from_nR(R, #n{sign="-", val=X}=Y, Env) ->
    from_nR(R, Y#n{sign=undefined, val=[$-|X]}, Env);
from_nR(R, #n{val=X}, _Env) ->
    {n, list_to_integer(X, R)}.

from_qR(R, #q{sign="-", num=X}=Y, Env) ->
    from_qR(R, Y#q{sign=undefined, num=[$-|X]}, Env);
from_qR(R, #q{num=X, den=Y}, _Env) ->
    {q, {list_to_integer(X, R), list_to_integer(Y, R)}}.

from_xR(R, #x{val=#n{sign=undefined, val=X}}, Env) ->
    from_xR1(R, #x{sign="+", val=X}, Env);
from_xR(R, #x{sign=undefined}=Y, Env) ->
    from_xR1(R, Y#x{sign="+"}, Env);
from_xR(R, Y, Env) ->
    from_xR1(R, Y, Env).

from_xR1(R, #x{val=X}=Y, Env) ->
    from_xR2(R, Y#x{val=string:strip(X, both, $0)}, Env).

from_xR2(R, #x{exp=undefined}=Y, Env) ->
    from_xR3(R, Y#x{exp="e+0"}, Env);
from_xR2(R, X, Env) ->
    from_xR3(R, X, Env).

from_xR3(R, #x{sign=S, val=X, exp=Y}, Env) when R==undefined; R==10 ->
    %% remove decimal point and exponent marker
    X1 = string:join(string:tokens(X, "."), ""),
    [Y1] = string:tokens(Y, "sfdle"),

    %% calculate power of 10
    P = case string:str(X, ".") of
            0 -> 0;
            I -> length(X) - I
        end,
    P1 = list_to_integer(Y1) - P,

    Num = case X1 of
              "" ->
                  0;
              _ ->
                  list_to_integer(S++X1)
          end,
    Den = erlang:trunc(math:pow(10, erlang:abs(P1))),

    %% simplify if possible
    if Num == 0 ->
            case S of
                "-" ->
                    {x, ?NZER};
                _ ->
                    {x, 0.0}
            end;
       P1 > 0 ->
            {x, Num * Den};
       true ->
            case scmtmp:'make-rational'(Num, Den, Env) of
                {Num1, 1} ->
                    {x, Num1};
                Q ->
                    {x, Q}
            end
    end.

from_in(_R, #in{sign="-", val=X}, _Env) ->
    {in, {-1, list_to_atom(X)}};
from_in(_R, #in{sign="+", val=X}, _Env) ->
    {in, {+1, list_to_atom(X)}}.

from_zpR(R, #zp{mag=X, ang=Y}, Env) ->
    {zp, {polar, {from_numR(R, X, Env), from_numR(R, Y, Env)}}}.

from_zrR(R, #zr{real=X, imag=Y}, Env) ->
    {zr, {rectangular, {from_numR(R, X, Env), from_numR(R, Y, Env)}}}.

to_exactness(undefined, {q, X}, Env) ->
    to_exactness1(e, X, Env);
to_exactness(undefined, {x, X}, Env) ->
    to_exactness1(i, X, Env);
to_exactness(E, {_, {polar, {X, Y}}}, Env) ->
    scmtmp:'make-polar'(to_exactness(E, X, Env), to_exactness(E, Y, Env), Env);
to_exactness(E, {_, {rectangular, {X, Y}}}, Env) ->
    scmtmp:'make-rectangular'(to_exactness(E, X, Env), to_exactness(E, Y, Env), Env);
to_exactness(E, {_, X}, Env) ->
    to_exactness1(E, X, Env);
to_exactness(E, X, Env) ->
    to_exactness1(E, X, Env).

to_exactness1(undefined, X, _Env) ->
    X;
to_exactness1(e, X, Env) ->
    scmtmp:exact(X, Env);
to_exactness1(i, X, Env) ->
    scmtmp:inexact(X, Env).
