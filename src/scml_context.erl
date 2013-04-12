%%% The MIT License
%%%
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation reads (the "Software"), to deal
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

-module(scml_context).

%% Imports
-export([imports/0]).

%% API
-export(['command-line'/0
         , 'exit'/0
         , 'exit'/1
         , 'emergency-exit'/0
         , 'emergency-exit'/1
         , 'get-environment-variable'/1
         , 'get-environment-variables'/0
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'command-line', #nip0{val=fun 'command-line'/0}}
     , {'exit', #nipn{val=[fun 'exit'/0, fun 'exit'/1]}}
     , {'emergency-exit', #nipn{val=[fun 'emergency-exit'/0, fun 'emergency-exit'/1]}}
     , {'get-environment-variable', #nipn{val=fun 'get-environment-variable'/1}}
     , {'get-environment-variables', #nip0{val=fun 'get-environment-variables'/0}}
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
