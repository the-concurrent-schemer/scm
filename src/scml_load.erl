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

%%% @doc Scheme load library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_load).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['load'/1
         , 'load'/2
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
    [{'load', #nipn{val=[fun 'load'/1, fun 'load'/2]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'load'(scm_string()) -> scm_false().
'load'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

-spec 'load'(scm_string(), scmi_env()) -> scm_false().
'load'(S, Env) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S, Env]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
