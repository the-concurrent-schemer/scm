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

-module(scml_base_symbol).

-include("scmi.hrl").

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['symbol?'/1
         , 'symbol=?'/1
         , 'symbol->string'/1
         , 'string->symbol'/1
        ]).

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'symbol?', #nipn{val=fun 'symbol?'/1}}
     , {'symbol=?', #nipv{val=fun 'symbol=?'/1}}
     , {'symbol->string', #nipn{val=fun 'symbol->string'/1}}
     , {'string->symbol', #nipn{val=fun 'string->symbol'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'symbol?'(scm_obj()) -> scm_boolean().
'symbol?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'symbol=?'([scm_symbol(),...]) -> scm_boolean().
'symbol=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Ss]).

-spec 'symbol->string'(scm_symbol()) -> scm_string().
'symbol->string'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'string->symbol'(scm_string()) -> scm_symbol().
'string->symbol'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
