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

%%% @doc Scheme base library for booleans
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_boolean).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['not'/1
        , 'boolean?'/1
        , 'boolean=?'/1
        ]).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'not', #nipn{val=fun ?MODULE:'not'/1}}
    , {'boolean?', #nipn{val=fun ?MODULE:'boolean?'/1}}
    , {'boolean=?', #nipv{val=fun ?MODULE:'boolean=?'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is false, and returns #f otherwise.
-spec 'not'(scm_obj()) -> scm_boolean().
'not'(?FALSE) ->
    ?TRUE;
'not'(_) ->
    ?FALSE.

%% @doc Returns #t if obj is either #t or #f, and returns #f
%% otherwise.
-spec 'boolean?'(scm_obj()) -> scm_boolean().
'boolean?'(?FALSE) ->
    ?TRUE;
'boolean?'(?TRUE) ->
    ?TRUE;
'boolean?'(_) ->
    ?FALSE.

%% @doc Returns #t if all the arguments are booleans and all are #t or
%% all are #f.
-spec 'boolean=?'([scm_boolean(),...]) -> scm_boolean().
'boolean=?'([]) ->
    ?TRUE;
'boolean=?'([?FALSE|Bs]) ->
    case lists:all(fun(?FALSE) -> true; (_) -> false end, Bs) of
        true ->
            ?TRUE;
        _ ->
            ?FALSE
    end;
'boolean=?'([?TRUE|Bs]) ->
    case lists:all(fun(?TRUE) -> true; (_) -> false end, Bs) of
        true ->
            ?TRUE;
        _ ->
            ?FALSE
    end;
'boolean=?'(_) ->
    ?FALSE.

%%%===================================================================
%%% internal helpers
%%%===================================================================
