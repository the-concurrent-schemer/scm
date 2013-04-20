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

Expect 1.

Nonterminals datum1 datums0 datums1 datum simple_datum compound_datum
symbol bytevector list vector abbreviation.

Terminals '#;' label labelref boolean number character string
identifier '(' ')' '#u8(' '#(' '.'  '\'' '`' ',' ',@'.

Rootsymbol datum1.

datum1 -> datum '#;' datum1 :
     '$1'.
datum1 -> '#;' datum1 datum :
     '$3'.
datum1 -> datum :
     '$1'.

datums0 -> '#;' datums0 :
     if '$2' /= [] ->
             tl('$2');
        true ->
             return_error(line_of('$1'), ["datum comment is invalid"])
     end.
datums0 -> datum datums0 :
     ['$1' | '$2'].
datums0 -> '$empty' :
     [].

datums1 -> '#;' datums1 :
     tl('$2').
datums1 -> datum datums1 :
     ['$1' | '$2'].
datums1 -> datum :
     ['$1'].

datum -> simple_datum :
     '$1'.
datum -> compound_datum :
     '$1'.
datum -> label datum1 :
     to_label(line_of('$1'), value_of('$1'), '$2').
datum -> labelref :
     to_labelref(line_of('$1'), value_of('$1')).

simple_datum -> boolean :
     to_boolean(line_of('$1'), value_of('$1')).
simple_datum -> number :
     to_number(line_of('$1'), value_of('$1')).
simple_datum -> character :
     to_character(line_of('$1'), value_of('$1')).
simple_datum -> string :
     to_string(line_of('$1'), value_of('$1')).
simple_datum -> symbol :
     to_symbol(line_of('$1'), value_of('$1')).
simple_datum -> bytevector :
     '$1'.

symbol -> identifier :
     '$1'.

bytevector -> '#u8(' datums0 ')' :
     to_bytevector(line_of('$1'), '$2').

compound_datum -> list :
     '$1'.
compound_datum -> vector :
     '$1'.
compound_datum -> abbreviation :
     '$1'.

list -> '(' datums0 ')' :
     to_list(line_of('$1'), '$2').
list ->  '(' datums1 '.' datum1 ')' :
     to_list(line_of('$1'), ['$2'|'$4']).

vector -> '#(' datums0 ')' :
     to_vector(line_of('$1'), '$2').

abbreviation -> '\'' datum1 :
     to_quote(line_of('$1'), ['$2']).
abbreviation -> '`' datum1 :
     to_quasiquote(line_of('$1'), ['$2']).
abbreviation -> ',' datum1 :
     to_unquote(line_of('$1'), ['$2']).
abbreviation -> ',@' datum1 :
     to_unquote_splicing(line_of('$1'), ['$2']).

Erlang code.

%% External exports
-export([binary/1
         , binary/2
         , binary/3
         , binary/4
         , iolist/1
         , iolist/2
         , iolist/3
         , iolist/4
         , string/1
         , string/2
         , string/3
         , string/4
         , file/1
         , file/3
        ]).

-export_type([filename/0, posix/0]).

-include("scmd.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type filename() :: file:filename().
-type posix() :: file:posix().

-record(state, {nested_comments_depth=0 :: non_neg_integer()}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec binary(binary()) -> {ok, term()} | {error, term()}.
binary(X) ->
    binary(X, 1).

-spec binary(binary(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
binary(X, LineNo) ->
    binary(scmd_scan, scmd_parse, X, LineNo).

-spec binary(Leex::module(), Yecc::module(), binary()) -> {ok, term()} | {error, term()}.
binary(Leex, Yecc, X) ->
    binary(Leex, Yecc, X, 1).

-spec binary(Leex::module(), Yecc::module(), binary(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
binary(Leex, Yecc, X, LineNo) ->
    string(Leex, Yecc, binary_to_list(X), LineNo).

-spec iolist(iolist()) -> {ok, term()} | {error, term()}.
iolist(X) ->
    iolist(X, 1).

-spec iolist(iolist(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
iolist(X, LineNo) ->
    iolist(scmd_scan, scmd_parse, X, LineNo).

-spec iolist(Leex::module(), Yecc::module(), iolist()) -> {ok, term()} | {error, term()}.
iolist(Leex, Yecc, X) ->
    iolist(Leex, Yecc, X, 1).

-spec iolist(Leex::module(), Yecc::module(), iolist(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
iolist(Leex, Yecc, X, LineNo) ->
    binary(Leex, Yecc, iolist_to_binary(X), LineNo).

-spec string(string()) -> {ok, term()} | {error, term()}.
string(X) ->
    string(scmd_scan, scmd_parse, X, 1).

-spec string(string(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
string(X, LineNo) ->
    string(scmd_scan, scmd_parse, X, LineNo).

-spec string(Leex::module(), Yecc::module(), string()) -> {ok, term()} | {error, term()}.
string(Leex, Yecc, X) ->
    string(Leex, Yecc, X, 1).

-spec string(Leex::module(), Yecc::module(), string(), LineNo::pos_integer()) -> {ok, term()} | {error, term()}.
string(Leex, Yecc, X, LineNo) ->
    Y = prefilter_string(X),
    case Leex:string(Y, LineNo) of
        {ok, Tokens, _EndLine} ->
            Yecc:parse(Tokens);
        Err ->
            Err
    end.

-spec file(filename()) -> {ok, term()} | {error, Reason} when
      Reason :: posix() | badarg | terminated | system_limit | term().
file(Filename) ->
    file(scmd_scan, scmd_parse, Filename).

-spec file(Leex::module(), Yecc::module(), filename()) -> {ok, term()} | {error, Reason} when
      Reason :: posix() | badarg | terminated | system_limit | term().
file(Leex, Yecc, Filename) ->
    case file:read_file(Filename) of
        {ok, X} ->
            binary(Leex, Yecc, X);
        Err ->
            Err
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% @TODO add error reporting for improper nested comments
-spec prefilter_string(string()) -> string().
prefilter_string(Str) ->
    prefilter_string(Str, [], #state{}).

prefilter_string([$#,$||T], L, S) ->
    skip_nested_comments(T, L, S);
prefilter_string([$;|T], L, S) ->
    skip_simple_comment(T, L, S);
prefilter_string([$#,H|T], L, S) when H >= $0, H =< $9 ->
    keep_label_or_labelref(T, [H,$#|L], S);
prefilter_string([$"|T], L, S) ->
    keep_string_literal(T, [$"|L], S);
prefilter_string([$||T], L, S) ->
    keep_identifier_literal(T, [$||L], S);
prefilter_string([$\\,H|T], L, S) when H==$x; H==$X ->
    keep_hex_scalar_value(T, [H,$\\|L], S);
prefilter_string([$#,$\\,$"|T], L, S) ->
    prefilter_string(T, [$\",$\\,$#|L], S);
prefilter_string([$#,$;|T], L, S) ->
                                prefilter_string(T, [$;,$#|L], S);
                             prefilter_string([$#,$\\,H|T], L, S) ->
                                prefilter_string(T, [H,$\\,$#|L], S);
                             prefilter_string([H|T], L, S) ->
                                prefilter_string(T, [H|L], S);
                             prefilter_string([], L, _S) ->
                                lists:reverse(L).

skip_nested_comments([$|,$#|T], L, #state{nested_comments_depth=0}=S) ->
    prefilter_string(T, L, S);
skip_nested_comments([$|,$#|T], L, #state{nested_comments_depth=N}=S) ->
    skip_nested_comments(T, L, S#state{nested_comments_depth=N-1});
skip_nested_comments([$#,$||T], L, #state{nested_comments_depth=N}=S) ->
    skip_nested_comments(T, L, S#state{nested_comments_depth=N+1});
skip_nested_comments([$\n|T], L, S)->
    skip_nested_comments(T, [$\n|L], S);
skip_nested_comments([_|T], L, S) ->
    skip_nested_comments(T, L, S).

skip_simple_comment([$\r,$\n|T], L, S)->
    prefilter_string(T, [$\r,$\n|L], S);
skip_simple_comment([$\r|T], L, S)->
    prefilter_string(T, [$\r|L], S);
skip_simple_comment([$\n|T], L, S)->
    prefilter_string(T, [$\n|L], S);
skip_simple_comment([_|T], L, S) ->
    skip_simple_comment(T, L, S);
skip_simple_comment([], L, S) ->
    prefilter_string([], L, S).

keep_label_or_labelref([H|T], L, S) when H >= $0, H =< $9 ->
    keep_label_or_labelref(T, [H|L], S);
keep_label_or_labelref([$#|T], L, S) ->
    prefilter_string(T, [$#|L], S);
keep_label_or_labelref(HT, L, S) ->
    prefilter_string(HT, L, S).

keep_string_literal([$"|T], L, S) ->
    prefilter_string(T, [$"|L], S);
keep_string_literal([$\\,H|T], L, S) ->
    keep_string_literal(T, [H,$\\|L], S);
keep_string_literal([H|T], L, S) ->
    keep_string_literal(T, [H|L], S).

keep_identifier_literal([$||T], L, S) ->
    prefilter_string(T, [$||L], S);
keep_identifier_literal([$\\,H|T], L, S) ->
    keep_identifier_literal(T, [H,$\\|L], S);
keep_identifier_literal([H|T], L, S) ->
    keep_identifier_literal(T, [H|L], S).

keep_hex_scalar_value([$;|T], L, S) ->
    prefilter_string(T, [$;|L], S);
keep_hex_scalar_value([H|T], L, S) when H >= $0, H =< $9 ->
    keep_hex_scalar_value(T, [H|L], S);
keep_hex_scalar_value([H|T], L, S) when H >= $a, H =< $f ->
    keep_hex_scalar_value(T, [H|L], S);
keep_hex_scalar_value([H|T], L, S) when H >= $A, H =< $F ->
    keep_hex_scalar_value(T, [H|L], S).

%%%----------------------------------------------------------------------
%%% Yecc functions
%%%----------------------------------------------------------------------

%% @TODO return_error display format isn't friendly

line_of(Token) ->
    element(2, Token).

value_of(Token) ->
    element(3, Token).

to_label(N, X, Y) ->
    #label{lineno=N, val={X, Y}}.

to_labelref(N, X) ->
    #labelref{lineno=N, val=X}.

to_boolean(N, X) ->
    #boolean{lineno=N, val=X}.

to_number(N, X) ->
    case scmd_parse_numR:to_number(N, X, scmi_env:the_empty()) of
        {ok, Y} ->
            Y;
        {error, N1, Msg} ->
            return_error(N1, Msg)
    end.

to_character(N, X) ->
    #character{lineno=N, val=X}.

to_string(N, X) ->
    #string{lineno=N, val=X}.

to_symbol(_N, X) ->
    X.

to_bytevector(N, X) ->
    Z = [ to_byte(N, Y) || Y <- X ],
    #bytevector{lineno=N, val=iolist_to_binary(Z)}.

to_byte(N, X) when is_integer(X), X < 0 ->
    return_error(N, ["byte is less than 0: ", io_lib:write(X)]);
to_byte(N, X) when is_integer(X), X > 255 ->
    return_error(N, ["byte is greater than 255: ", io_lib:write(X)]);
to_byte(_N, X) when is_integer(X) ->
    X;
to_byte(N, X) ->
    return_error(N, ["byte is invalid: ", io_lib:write(X)]).

to_list(N, ['quote']) ->
    return_error(N, ["quote is empty"]);
to_list(N, ['quasiquote']) ->
    return_error(N, ["quasiquote is empty"]);
to_list(N, ['unquote']) ->
    return_error(N, ["unquote is empty"]);
to_list(N, ['unquote-splicing']) ->
    return_error(N, ["unquote-splicing is empty"]);
to_list(N, ['quote'|T]) ->
    to_quote(N, T);
to_list(N, ['quasiquote'|T]) ->
    to_quasiquote(N, T);
to_list(N, ['unquote'|T]) ->
    to_unquote(N, T);
to_list(N, ['unquote-splicing'|T]) ->
    to_unquote_splicing(N, T);
to_list(_N, X) ->
    X.

to_vector(N, X) ->
    #vector{lineno=N, val=list_to_tuple(X)}.

to_quote(N, X) ->
    #quote{lineno=N, val=X}.

to_quasiquote(N, X) ->
    #quasiquote{lineno=N, val=X}.

to_unquote(N, X) ->
    #unquote{lineno=N, val=X}.

to_unquote_splicing(N, X) ->
    #unquote_splicing{lineno=N, val=X}.
