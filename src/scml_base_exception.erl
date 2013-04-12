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

-module(scml_base_exception).

%% Imports
-export([imports/0]).

%% API
-export(['with-exception-handler'/2
         , 'raise'/1
         , 'raise-continuable'/1
         , 'error'/1
         , 'error-object?'/1
         , 'error-object-message'/1
         , 'error-object-irritants'/1
         , 'read-error?'/1
         , 'file-error?'/1
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'with-exception-handler', #nipn{val=fun 'with-exception-handler'/2}}
     , {'raise', #nipn{val=fun 'raise'/1}}
     , {'raise-continuable', #nipn{val=fun 'raise-continuable'/1}}
     , {'error', #nipv{val=fun 'error'/1}}
     , {'error-object?', #nipn{val=fun 'error-object?'/1}}
     , {'error-object-message', #nipn{val=fun 'error-object-message'/1}}
     , {'error-object-irritants', #nipn{val=fun 'error-object-irritants'/1}}
     , {'read-error?', #nipn{val=fun 'read-error?'/1}}
     , {'file-error?', #nipn{val=fun 'file-error?'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'with-exception-handler'(scm_proc(), scm_thunk()) -> scm_obj().
'with-exception-handler'(Handler, Thunk) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Handler, Thunk]).

-spec 'raise'(scm_obj()) -> scm_obj().
'raise'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'raise-continuable'(scm_obj()) -> scm_obj().
'raise-continuable'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'error'([scm_obj(),...]) -> scm_obj().
'error'([Message|Objs]) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Message|Objs]).

-spec 'error-object?'(scm_obj()) -> scm_boolean().
'error-object?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'error-object-message'(scm_obj()) -> scm_obj().
'error-object-message'(ErrObj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [ErrObj]).

-spec 'error-object-irritants'(scm_obj()) -> scm_obj().
'error-object-irritants'(ErrObj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [ErrObj]).

-spec 'read-error?'(scm_obj()) -> scm_boolean().
'read-error?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'file-error?'(scm_obj()) -> scm_boolean().
'file-error?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
