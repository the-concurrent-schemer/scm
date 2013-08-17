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

%%% @doc Scheme file library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_file).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['call-with-input-file'/2
         , 'call-with-output-file'/2
         , 'with-input-from-file'/2
         , 'with-output-to-file'/2
         , 'open-input-file'/1
         , 'open-binary-input-file'/1
         , 'open-output-file'/1
         , 'open-binary-output-file'/1

         , 'file-exists?'/1
         , 'delete-file'/1
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
    [{'call-with-input-file', #nipn{val=fun ?MODULE:'call-with-input-file'/2}}
     , {'call-with-output-file', #nipn{val=fun ?MODULE:'call-with-output-file'/2}}
     , {'with-input-from-file', #nipn{val=fun ?MODULE:'with-input-from-file'/2}}
     , {'with-output-to-file', #nipn{val=fun ?MODULE:'with-output-to-file'/2}}
     , {'open-input-file', #nipn{val=fun ?MODULE:'open-input-file'/1}}
     , {'open-binary-input-file', #nipn{val=fun ?MODULE:'open-binary-input-file'/1}}
     , {'open-output-file', #nipn{val=fun ?MODULE:'open-output-file'/1}}
     , {'open-binary-output-file', #nipn{val=fun ?MODULE:'open-binary-output-file'/1}}
     , {'file-exists?', #nipn{val=fun ?MODULE:'file-exists?'/1}}
     , {'delete-file', #nipn{val=fun ?MODULE:'delete-file'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'call-with-input-file'(scm_string(), scm_proc()) -> scm_obj().
'call-with-input-file'(S, Proc) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Proc]).

-spec 'call-with-output-file'(scm_string(), scm_proc()) -> scm_obj().
'call-with-output-file'(S, Proc) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Proc]).

-spec 'with-input-from-file'(scm_string(), scm_thunk()) -> scm_obj().
'with-input-from-file'(S, Thunk) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Thunk]).

-spec 'with-output-to-file'(scm_string(), scm_thunk()) -> scm_obj().
'with-output-to-file'(S, Thunk) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Thunk]).

-spec 'open-input-file'(scm_string()) -> scm_port().
'open-input-file'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'open-binary-input-file'(scm_string()) -> scm_port().
'open-binary-input-file'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'open-output-file'(scm_string()) -> scm_port().
'open-output-file'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'open-binary-output-file'(scm_string()) -> scm_port().
'open-binary-output-file'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

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
