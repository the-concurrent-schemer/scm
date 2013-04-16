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

-module(scml_write).

-include("scmi.hrl").

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['write'/1
         , 'write'/2
         , 'write-shared'/1
         , 'write-shared'/2
         , 'write-simple'/1
         , 'write-simple'/2
         , 'display'/1
         , 'display'/2
        ]).

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'write', #nipn{val=[fun 'write'/1, fun 'write'/2]}}
     , {'write-shared', #nipn{val=[fun 'write-shared'/1, fun 'write-shared'/2]}}
     , {'write-simple', #nipn{val=[fun 'write-simple'/1, fun 'write-simple'/2]}}
     , {'display', #nipn{val=[fun 'display'/1, fun 'display'/2]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'write'(scm_obj()) -> scm_false().
'write'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'write'(scm_obj(), scm_port()) -> scm_false().
'write'(Obj, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj, Port]).

-spec 'write-shared'(scm_obj()) -> scm_false().
'write-shared'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'write-shared'(scm_obj(), scm_port()) -> scm_false().
'write-shared'(Obj, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj, Port]).

-spec 'write-simple'(scm_obj()) -> scm_false().
'write-simple'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'write-simple'(scm_obj(), scm_port()) -> scm_false().
'write-simple'(Obj, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj, Port]).

-spec 'display'(scm_obj()) -> scm_false().
'display'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj]).

-spec 'display'(scm_obj(), scm_port()) -> scm_false().
'display'(Obj, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Obj, Port]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
