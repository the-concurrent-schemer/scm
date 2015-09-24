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

%%% @doc Scheme process-context library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_context).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['command-line'/0
        , 'exit'/0
        , 'exit'/1
        , 'emergency-exit'/0
        , 'emergency-exit'/1
        , 'get-environment-variable'/1
        , 'get-environment-variables'/0
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
    [{'command-line', #nip0{val=fun ?MODULE:'command-line'/0}}
    , {'exit', #nipn{val=[fun ?MODULE:'exit'/0, fun ?MODULE:'exit'/1]}}
    , {'emergency-exit', #nipn{val=[fun ?MODULE:'emergency-exit'/0, fun ?MODULE:'emergency-exit'/1]}}
    , {'get-environment-variable', #nipn{val=fun ?MODULE:'get-environment-variable'/1}}
    , {'get-environment-variables', #nip0{val=fun ?MODULE:'get-environment-variables'/0}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'command-line'() -> [scm_string(),...].
'command-line'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'exit'() -> no_return().
'exit'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'exit'(scm_obj()) -> no_return().
'exit'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'emergency-exit'() -> no_return().
'emergency-exit'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'emergency-exit'(scm_obj()) -> no_return().
'emergency-exit'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'get-environment-variable'(scm_string()) -> scm_string() | scm_false().
'get-environment-variable'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

-spec 'get-environment-variables'() -> [[scm_string()|scm_string()]].
'get-environment-variables'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

%%%===================================================================
%%% internal helpers
%%%===================================================================
