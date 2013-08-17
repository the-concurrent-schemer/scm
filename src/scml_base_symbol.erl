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

%%% @doc Scheme base library for symbols
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_base_symbol).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['symbol?'/1
         , 'symbol=?'/1
         , 'symbol->string'/1
         , 'string->symbol'/1
        ]).

%% Internal API
-export([symbol_to_unicode/1, unicode_to_symbol/1]).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'symbol?', #nipn{val=fun ?MODULE:'symbol?'/1}}
     , {'symbol=?', #nipv{val=fun ?MODULE:'symbol=?'/1}}
     , {'symbol->string', #nipn{val=fun ?MODULE:'symbol->string'/1}}
     , {'string->symbol', #nipn{val=fun ?MODULE:'string->symbol'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a symbol, otherwise returns #f.
-spec 'symbol?'(scm_obj()) -> scm_boolean().
'symbol?'(Obj) when is_atom(Obj) ->
    ?TRUE;
'symbol?'({SHA, Obj}) when is_atom(SHA), is_binary(Obj) ->
    ?TRUE;
'symbol?'(_) ->
    ?FALSE.

%% @doc Returns #t if all the arguments are symbols and all have the
%% same names in the sense of string=?.
-spec 'symbol=?'([scm_symbol(),...]) -> scm_boolean().
'symbol=?'([]) ->
    ?TRUE;
'symbol=?'([S|Ss]) ->
    case 'symbol?'(S) of
        ?FALSE ->
            ?FALSE;
        _ ->
            Fun = fun(X) -> S =:= X end,
            case lists:all(Fun, Ss) of
                true ->
                    ?TRUE;
                _ ->
                    ?FALSE
            end
    end.

%% @doc Returns the name of symbol as a string, but without adding
%% escapes.
-spec 'symbol->string'(scm_symbol()) -> scm_string().
'symbol->string'(S) ->
    #string{val=list_to_tuple(symbol_to_unicode(S))}.

%% @doc Returns the symbol whose name is string.  This procedure can
%% create symbols with names containing special characters that would
%% require escaping with written, but does not interpret escapes in
%% its input.
-spec 'string->symbol'(scm_string()) -> scm_symbol().
'string->symbol'(#string{val=S}) ->
    unicode_to_symbol(tuple_to_list(S)).

%%%===================================================================
%%% internal helpers
%%%===================================================================

-spec symbol_to_unicode(scm_symbol()) -> [scmd_types_impl:unichar()].
symbol_to_unicode(S) when is_atom(S) ->
    scml:to_unicode(atom_to_binary(S, utf8));
symbol_to_unicode({SHA, S}) when is_atom(SHA), is_binary(S) ->
    scml:to_unicode(S).

-spec unicode_to_symbol([scmd_types_impl:unichar()]) -> scm_symbol().
unicode_to_symbol(L) ->
    X = scml:to_utf8(L),
    try
        Y = binary_to_atom(X, utf8),
        case atom_to_list(Y) of
            Z when length(Z) < 20 ->
                Y;
            _ ->
                {binary_to_atom(crypto:hash(sha, X), latin1), X}
        end
    catch error:badarg ->
            {binary_to_atom(crypto:hash(sha, X), latin1), X}
    end.
