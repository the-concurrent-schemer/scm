%%% The MIT License
%%%
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Parse transform to export function imports
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(xfm_import_as_export).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

parse_transform(Forms, _Options) ->
    forms(Forms).

%%%===================================================================
%%% Internal functions
%%%===================================================================

forms(L) ->
    forms(L, []).

forms([{attribute,Ln,import_as_export,{Module,Imports}=H}|T], Acc) when is_atom(Module), is_list(Imports) ->
    %% replace import_as_export with export
    Attr = {attribute,Ln,export,Imports},
    [Attr|forms(T, [H|Acc])];
forms([{eof,Ln}|_], Acc) ->
    forms(Ln, Acc, [{eof,Ln}]);
forms([H|T], Acc) ->
    [H|forms(T, Acc)].

forms(Ln, [{Module,Imports}|T], Acc) ->
    forms(Ln, T, [import_as_export(Ln, Module, Imports)|Acc]);
forms(_Ln, [], Acc) ->
    lists:flatten(Acc).

import_as_export(Ln, Module, Imports) ->
    [ import_as_export(Ln, Module, Name, Arity) || {Name,Arity} <- Imports ].

import_as_export(Ln, Module, Name, Arity) when is_atom(Module), is_atom(Name), is_integer(Arity) ->
    Vars = [{var,Ln,integer_to_var(V)} || V <- lists:seq(1,Arity)],
    Body = {call,Ln,{remote,Ln,{atom,Ln,Module},{atom,Ln,Name}},Vars},
    {function,Ln,Name,Arity,[{clause,Ln,Vars,[],[Body]}]}.

integer_to_var(X) ->
    list_to_atom([$V|integer_to_list(X)]).
