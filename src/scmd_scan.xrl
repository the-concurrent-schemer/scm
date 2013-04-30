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

%%% @doc Scheme datum tokenizer
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

Definitions.

%% token - see LALR-1 parser generator

A = [aA]
B = [bB]
C = [cC]
D = [dD]
E = [eE]
F = [fF]
%%G = [gG]
%%H = [hH]
I = [iI]
%%J = [jJ]
%%K = [kK]
L = [lL]
%%M = [mM]
N = [nN]
O = [oO]
%%P = [pP]
%%Q = [qQ]
R = [rR]
S = [sS]
T = [tT]
U = [uU]
%%V = [vV]
%%W = [wW]
X = [xX]
%%Y = [yY]
%%Z = [zZ]

intraline_whitespace = [\s\t]

whitespace           = ({intraline_whitespace}|{line_ending})

vertical_line        = \|

line_ending          = (\n|\r\n|\r)

%% comment and comment datum - see below
%% nested comment MUST be removed by a pre-processor

%% directive - see below

%% atmosphere and intertoken space - see LALR-1 parser generator

identifier           = ({initial}{subsequent}*|{vertical_line}{symbol_element}*{vertical_line}|{peculiar_identifier})

initial              = ({letter}|{special_initial})

letter               = [a-zA-Z]

special_initial      = [!$%&*/:<=>?^_~]

subsequent           = ({initial}|{digit}|{special_subsequent})

digit                = [0-9]

hex_digit            = [0-9a-fA-F]

explicit_sign        = [+-]

special_subsequent   = ({explicit_sign}|\.|@)

inline_hex_escape    = (\\{X}{hex_scalar_value};)

hex_scalar_value     = 0*(([0-9a-eA-e]{hex_digit}{hex_digit}{hex_digit})|({D}{digit8}{hex_digit}{hex_digit})|(({E}|{F}){hex_digit}{hex_digit}{hex_digit})|10{hex_digit}{hex_digit}{hex_digit}{hex_digit})

mnemonic_escape      = \\(a|b|t|n|r)

peculiar_identifier  = ({explicit_sign}|{explicit_sign}{sign_subsequent}{subsequent}*|{explicit_sign}\.{dot_subsequent}{subsequent}*|\.{dot_subsequent}{subsequent}*)

dot_subsequent       = ({sign_subsequent}|\.)

sign_subsequent      = ({initial}|{explicit_sign}|@)

symbol_element       = ([^|\\]|{mnemonic_escape}|\\\||{inline_hex_escape})

boolean_false        = #({F}{A}{L}{S}{E}|{F})
boolean_true         = #({T}{R}{U}{E}|{T})

character            = #\\({character_any}|{character_name}|{X}{hex_scalar_value})

character_any        = (.|\n)

character_name       = (alarm|backspace|delete|escape|newline|null|return|space|tab)

string               = "{string_element}*"

string_element       = ([^"\\]|{mnemonic_escape}|\\("|\\|{intraline_whitespace}*{line_ending}{intraline_whitespace}*)|{inline_hex_escape})

%% bytevector and byte - see LALR-1 parser generator

%% number - see LALR-1 parser generator

number               = ({num2}|{num8}|{num10}|{num16})

num2                 = ({prefix2}{complex2})
num8                 = ({prefix8}{complex8})
num10                = ({prefix10}{complex10})
num16                = ({prefix16}{complex16})

complex2             = ({polar2}|{rectangular2}|{real2})
complex8             = ({polar8}|{rectangular8}|{real8})
complex10            = ({polar10}|{rectangular10}|{real10})
complex16            = ({polar16}|{rectangular16}|{real16})

polar2               = {real2}@{real2}
polar8               = {real8}@{real8}
polar10              = {real10}@{real10}
polar16              = {real16}@{real16}

rectangular2         = (({real2}([+-]({ureal2}|{infnan})?)?)|[+-]){I}
rectangular8         = (({real8}([+-]({ureal8}|{infnan})?)?)|[+-]){I}
rectangular10        = (({real10}([+-]({ureal10}|{infnan})?)?)|[+-]){I}
rectangular16        = (({real16}([+-]({ureal16}|{infnan})?)?)|[+-]){I}

real2                = ({sign}{ureal2}|[+-]{infnan})
real8                = ({sign}{ureal8}|[+-]{infnan})
real10               = ({sign}{ureal10}|[+-]{infnan})
real16               = ({sign}{ureal16}|[+-]{infnan})

ureal2               = ({uinteger2}|{uinteger2}/{uinteger2})
ureal8               = ({uinteger8}|{uinteger8}/{uinteger8})
ureal10              = ({uinteger10}|{uinteger10}/{uinteger10}|{decimal10})
ureal16              = ({uinteger16}|{uinteger16}/{uinteger16})

decimal10            = ({uinteger10}|\.{digit10}+|{digit10}+\.{digit10}*){suffix}?

uinteger2            = {digit2}+
uinteger8            = {digit8}+
uinteger10           = {digit10}+
uinteger16           = {digit16}+

prefix2              = ({radix2}{exactness}|{exactness}{radix2})
prefix8              = ({radix8}{exactness}|{exactness}{radix8})
prefix10             = ({radix10}{exactness}|{exactness}{radix10})
prefix16             = ({radix16}{exactness}|{exactness}{radix16})

infnan               = ({I}{N}{F}|{N}{A}{N})\.0

suffix               = ({exponent_marker}{sign}{digit10}+)?

exponent_marker      = ({E}|{S}|{F}|{D}|{L})

sign                 = [+-]?

exactness            = (#{I}|#{E})?

radix2               = #{B}
radix8               = #{O}
radix10              = (#{D})?
radix16              = #{X}

digit2               = [0-1]
digit8               = [0-7]
digit10              = [0-9]
digit16              = [0-9a-fA-F]

label                = #{uinteger10}

Rules.

%% boolean
{boolean_false}                    : {token, {boolean, TokenLine, false}}.
{boolean_true}                     : {token, {boolean, TokenLine, true}}.

%% number
{number}                           : {token, {number, TokenLine, TokenChars}}.

%% character
{character}                        : {token, {character, TokenLine, to_character(TokenLine, TokenChars)}}.

%% string
{string}                           : {token, {string, TokenLine, to_string(TokenLine, TokenChars)}}.

%% label
{label}=                           : {token, {label, TokenLine, to_label(TokenChars)}}.
{label}#                           : {token, {labelref, TokenLine, to_labelref(TokenChars)}}.

%% token
\(                                 : {token, {'(', TokenLine}}.
\)                                 : {token, {')', TokenLine}}.
#\(                                : {token, {'#(', TokenLine}}.
#{U}8\(                            : {token, {'#u8(', TokenLine}}.
'                                  : {token, {'\'', TokenLine}}.
`                                  : {token, {'`', TokenLine}}.
,@                                 : {token, {',@', TokenLine}}.
,                                  : {token, {',', TokenLine}}.
\.                                 : {token, {'.', TokenLine}}.

%% identifier
{identifier}                       : to_identifier(TokenLine, TokenChars).

%% directive
%% @TODO unsupported
#!{F}{O}{L}{D}-{C}{A}{S}{E}        : skip_token.
#!{N}{O}-{F}{O}{L}{D}-{C}{A}{S}{E} : skip_token.

%% simple and datum comment
;[^\r\n]*{line_ending}             : skip_token.
#;                                 : {token, {'#;', TokenLine}}.

%% white space
{whitespace}                       : skip_token.

Erlang code.

-include("scmd_types_impl.hrl").

%%%----------------------------------------------------------------------
%%% Leex functions
%%%----------------------------------------------------------------------

unescape(N, X) ->
    Opts = [unicode, {return,list}],
    Fun = fun([[C]=M], Acc) when C==$a; C==$A ->
                  R = ?ALARM,
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R]);
             ([[C]=M], Acc) when C==$b; C==$B ->
                  R = ?BACKSPACE,
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R]);
             ([[C]=M], Acc) when C==$t; C==$T ->
                  R = ?TAB,
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R]);
             ([[C]=M], Acc) when C==$n; C==$N ->
                  R = ?NEWLINE,
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R]);
             ([[C]=M], Acc) when C==$r; C==$R ->
                  R = ?RETURN,
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R]);
             ([[$"]], Acc) ->
                  R = ?DQUOTE,
                  string:join(re:split(Acc, "\\\\\\\"", Opts), [R]);
             ([[$\\]], Acc) ->
                  R = ?BACKSLASH,
                  string:join(re:split(Acc, "\\\\\\\\", Opts), [R]);
             ([[C|L]=M], Acc) when C==$x; C==$X ->
                  R = hexstring_to_unichar(N, string:substr(L, 1, length(L) - 1)),
                  string:join(re:split(Acc, "\\\\"++M, Opts), [R])
          end,

    Opts1 = [unicode, caseless, multiline, global, {capture,all_but_first,list}],
    Y = case re:run(X, "\\\\(x[a-f0-9]+;)", Opts1) of
            nomatch -> X;
            {match, Xs} -> lists:foldl(Fun, X, Xs)
        end,
    case re:run(Y, "\\\\(a|b|t|n|r|\"|\\\\)", Opts1) of
        nomatch -> Y;
        {match, Ys} -> lists:foldl(Fun, Y, Ys)
    end.

hexstring_to_unichar(N, X) ->
    case list_to_integer(X, 16) of
        Y when Y >= ?UNICHAR_LOW_MIN, Y =< ?UNICHAR_LOW_MAX -> Y;
        Y when Y >= ?UNICHAR_HIGH_MIN, Y =< ?UNICHAR_HIGH_MAX -> Y;
        _ -> {N, ?MODULE, io_lib:format("Invalid hexstring: ~w", [X])}
    end.

to_character(N, X) ->
    case string:substr(X, 3) of
        [Y] when Y >= ?UNICHAR_LOW_MIN, Y =< ?UNICHAR_LOW_MAX -> Y;
        [Y] when Y >= ?UNICHAR_HIGH_MIN, Y =< ?UNICHAR_HIGH_MAX -> Y;
        "alarm"     -> ?ALARM;
        "backspace" -> ?BACKSPACE;
        "delete"    -> ?DELETE;
        "escape"    -> ?ESCAPE;
        "newline"   -> ?NEWLINE;
        "null"      -> ?NULL;
        "return"    -> ?RETURN;
        "space"     -> ?SPACE;
        "tab"       -> ?TAB;
        [C|Y] when C==$x; C==$X -> hexstring_to_unichar(N, Y);
        _ -> {N, ?MODULE, io_lib:format("Invalid character: ~w", [X])}
    end.

to_string(N, X) ->
    Y = unescape(N, string:substr(X, 2, length(X) - 2)),
    list_to_tuple(unicode:characters_to_list(Y, unicode)).

to_label(X) ->
    list_to_integer(string:substr(X, 2, length(X) - 2)).

to_labelref(X) ->
    list_to_integer(string:substr(X, 2, length(X) - 2)).

to_identifier(N, X) ->
    Y = unescape(N, X),
    Opts = [unicode, caseless, {capture,all_but_first,index}],
    case re:run(Y, "^(\\+i|\\-i|\\+inf\\.0|\\-inf\\.0|\\+nan\\.0|\\-nan\\.0)$", Opts) of
        nomatch ->
            {token, {identifier, N, to_identifier_atom(N, Y)}};
        {match, [{0,Len}]} when Len==length(Y) ->
            {token, {number, N, Y}}
    end.

to_identifier_atom(_N, "-nan.0") ->
    to_identifier_atom(_N, "+nan.0");
to_identifier_atom(_N, X) ->
    scml_base_symbol:unicode_to_symbol(X).
