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

-module(scml_base_char).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['char?'/1
         , 'char=?'/1
         , 'char<?'/1
         , 'char>?'/1
         , 'char<=?'/1
         , 'char>=?'/1
         , 'char->integer'/1
         , 'integer->char'/1
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
    [{'char?', #nipn{val=fun 'char?'/1}}
     , {'char=?', #nipv{val=fun 'char=?'/1}}
     , {'char<?', #nipv{val=fun 'char<?'/1}}
     , {'char>?', #nipv{val=fun 'char>?'/1}}
     , {'char<=?', #nipv{val=fun 'char<=?'/1}}
     , {'char>=?', #nipv{val=fun 'char>=?'/1}}
     , {'char->integer', #nipn{val=fun 'char->integer'/1}}
     , {'integer->char', #nipn{val=fun 'integer->char'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'char?'(scm_obj()) -> scm_boolean().
'char?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'char=?'([scm_char(),...]) -> scm_boolean().
'char=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

-spec 'char<?'([scm_char(),...]) -> scm_boolean().
'char<?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

-spec 'char>?'([scm_char(),...]) -> scm_boolean().
'char>?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

-spec 'char<=?'([scm_char(),...]) -> scm_boolean().
'char<=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

-spec 'char>=?'([scm_char(),...]) -> scm_boolean().
'char>=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Cs]).

-spec 'char->integer'(scm_char()) -> scm_n().
'char->integer'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [C]).

-spec 'integer->char'(scm_n()) -> scm_char().
'integer->char'(N) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [N]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
