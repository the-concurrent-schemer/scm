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

-module(scml_file).

%% Imports
-export([imports/0]).

%% API
-export(['file-exists?'/1
         , 'delete-file'/1
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'file-exists?', #nipn{val=fun 'file-exists?'/1}}
     , {'delete-file', #nipn{val=fun 'delete-file'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'file-exists?'(scm_string()) -> scm_boolean().
'file-exists?'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

-spec 'delete-file'(scm_string()) -> scm_false().
'delete-file'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
