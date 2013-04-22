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

%%% @doc Scheme char library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_char).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['char-ci=?'/1
         , 'char-ci<?'/1
         , 'char-ci>?'/1
         , 'char-ci<=?'/1
         , 'char-ci>=?'/1
         , 'char-alphabetic?'/1
         , 'char-numeric?'/1
         , 'char-whitespace?'/1
         , 'char-upper-case?'/1
         , 'char-lower-case?'/1
         , 'digit-value'/1
         , 'char-upcase'/1
         , 'char-downcase'/1
         , 'char-foldcase'/1
         , 'string-ci=?'/1
         , 'string-ci<?'/1
         , 'string-ci>?'/1
         , 'string-ci<=?'/1
         , 'string-ci>=?'/1
         , 'string-upcase'/1
         , 'string-downcase'/1
         , 'string-foldcase'/1
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
    [{'char-ci=?', #nipv{val=fun 'char-ci=?'/1}}
     , {'char-ci<?', #nipv{val=fun 'char-ci<?'/1}}
     , {'char-ci>?', #nipv{val=fun 'char-ci>?'/1}}
     , {'char-ci<=?', #nipv{val=fun 'char-ci<=?'/1}}
     , {'char-ci>=?', #nipv{val=fun 'char-ci>=?'/1}}
     , {'char-alphabetic?', #nipn{val=fun 'char-alphabetic?'/1}}
     , {'char-numeric?', #nipn{val=fun 'char-numeric?'/1}}
     , {'char-whitespace?', #nipn{val=fun 'char-whitespace?'/1}}
     , {'char-upper-case?', #nipn{val=fun 'char-upper-case?'/1}}
     , {'char-lower-case?', #nipn{val=fun 'char-lower-case?'/1}}
     , {'digit-value', #nipn{val=fun 'digit-value'/1}}
     , {'char-upcase', #nipn{val=fun 'char-upcase'/1}}
     , {'char-downcase', #nipn{val=fun 'char-downcase'/1}}
     , {'char-foldcase', #nipn{val=fun 'char-foldcase'/1}}
     , {'string-ci=?', #nipv{val=fun 'string-ci=?'/1}}
     , {'string-ci<?', #nipv{val=fun 'string-ci<?'/1}}
     , {'string-ci>?', #nipv{val=fun 'string-ci>?'/1}}
     , {'string-ci<=?', #nipv{val=fun 'string-ci<=?'/1}}
     , {'string-ci>=?', #nipv{val=fun 'string-ci>=?'/1}}
     , {'string-upcase', #nipn{val=fun 'string-upcase'/1}}
     , {'string-downcase', #nipn{val=fun 'string-downcase'/1}}
     , {'string-foldcase', #nipn{val=fun 'string-foldcase'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'char-ci=?'([scm_char(),...]) -> scm_boolean().
'char-ci=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Cs]).

-spec 'char-ci<?'([scm_char(),...]) -> scm_boolean().
'char-ci<?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Cs]).

-spec 'char-ci>?'([scm_char(),...]) -> scm_boolean().
'char-ci>?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Cs]).

-spec 'char-ci<=?'([scm_char(),...]) -> scm_boolean().
'char-ci<=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Cs]).

-spec 'char-ci>=?'([scm_char(),...]) -> scm_boolean().
'char-ci>=?'(Cs) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Cs]).

-spec 'char-alphabetic?'(scm_char()) -> scm_boolean().
'char-alphabetic?'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-numeric?'(scm_char()) -> scm_boolean().
'char-numeric?'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-whitespace?'(scm_char()) -> scm_boolean().
'char-whitespace?'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-upper-case?'(scm_letter()) -> scm_boolean().
'char-upper-case?'(L) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [L]).

-spec 'char-lower-case?'(scm_letter()) -> scm_boolean().
'char-lower-case?'(L) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [L]).

-spec 'digit-value'(scm_char()) -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | scm_false().
'digit-value'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-upcase'(scm_char()) -> scm_char().
'char-upcase'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-downcase'(scm_char()) -> scm_char().
'char-downcase'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'char-foldcase'(scm_char()) -> scm_char().
'char-foldcase'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [C]).

-spec 'string-ci=?'([scm_string(),...]) -> scm_boolean().
'string-ci=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Ss]).

-spec 'string-ci<?'([scm_string(),...]) -> scm_boolean().
'string-ci<?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Ss]).

-spec 'string-ci>?'([scm_string(),...]) -> scm_boolean().
'string-ci>?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Ss]).

-spec 'string-ci<=?'([scm_string(),...]) -> scm_boolean().
'string-ci<=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Ss]).

-spec 'string-ci>=?'([scm_string(),...]) -> scm_boolean().
'string-ci>=?'(Ss) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Ss]).

-spec 'string-upcase'(scm_string()) -> scm_string().
'string-upcase'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

-spec 'string-downcase'(scm_string()) -> scm_string().
'string-downcase'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

-spec 'string-foldcase'(scm_string()) -> scm_string().
'string-foldcase'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [S]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
