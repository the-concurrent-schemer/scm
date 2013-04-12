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

-module(scml_cxr).

%% Imports
-export([imports/0]).

%% API
-export([caaar/1
         , caadr/1
         , cadar/1
         , caddr/1
         , cdaar/1
         , cdadr/1
         , cddar/1
         , cdddr/1
         , caaaar/1
         , caaadr/1
         , caadar/1
         , caaddr/1
         , cadaar/1
         , cadadr/1
         , caddar/1
         , cadddr/1
         , cdaaar/1
         , cdaadr/1
         , cdadar/1
         , cdaddr/1
         , cddaar/1
         , cddadr/1
         , cdddar/1
         , cddddr/1
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{caaar, #nipn{val=fun caaar/1}}
     , {caadr, #nipn{val=fun caadr/1}}
     , {cadar, #nipn{val=fun cadar/1}}
     , {caddr, #nipn{val=fun caddr/1}}
     , {cdaar, #nipn{val=fun cdaar/1}}
     , {cdadr, #nipn{val=fun cdadr/1}}
     , {cddar, #nipn{val=fun cddar/1}}
     , {cdddr, #nipn{val=fun cdddr/1}}
     , {caaaar, #nipn{val=fun caaaar/1}}
     , {caaadr, #nipn{val=fun caaadr/1}}
     , {caadar, #nipn{val=fun caadar/1}}
     , {caaddr, #nipn{val=fun caaddr/1}}
     , {cadaar, #nipn{val=fun cadaar/1}}
     , {cadadr, #nipn{val=fun cadadr/1}}
     , {caddar, #nipn{val=fun caddar/1}}
     , {cadddr, #nipn{val=fun cadddr/1}}
     , {cdaaar, #nipn{val=fun cdaaar/1}}
     , {cdaadr, #nipn{val=fun cdaadr/1}}
     , {cdadar, #nipn{val=fun cdadar/1}}
     , {cdaddr, #nipn{val=fun cdaddr/1}}
     , {cddaar, #nipn{val=fun cddaar/1}}
     , {cddadr, #nipn{val=fun cddadr/1}}
     , {cdddar, #nipn{val=fun cdddar/1}}
     , {cddddr, #nipn{val=fun cddddr/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec caaar(scm_pair()) -> scm_obj().
caaar(Pair) ->
    scml_base_list:car(scml_base_list:caar(Pair)).

-spec caadr(scm_pair()) -> scm_obj().
caadr(Pair) ->
    scml_base_list:car(scml_base_list:cadr(Pair)).

-spec cadar(scm_pair()) -> scm_obj().
cadar(Pair) ->
    scml_base_list:car(scml_base_list:cdar(Pair)).

-spec caddr(scm_pair()) -> scm_obj().
caddr(Pair) ->
    scml_base_list:car(scml_base_list:cddr(Pair)).

-spec cdaar(scm_pair()) -> scm_obj().
cdaar(Pair) ->
    scml_base_list:cdr(scml_base_list:caar(Pair)).

-spec cdadr(scm_pair()) -> scm_obj().
cdadr(Pair) ->
    scml_base_list:cdr(scml_base_list:cadr(Pair)).

-spec cddar(scm_pair()) -> scm_obj().
cddar(Pair) ->
    scml_base_list:cdr(scml_base_list:cdar(Pair)).

-spec cdddr(scm_pair()) -> scm_obj().
cdddr(Pair) ->
    scml_base_list:cdr(scml_base_list:cddr(Pair)).

-spec caaaar(scm_pair()) -> scm_obj().
caaaar(Pair) ->
    scml_base_list:car(caaar(Pair)).

-spec caaadr(scm_pair()) -> scm_obj().
caaadr(Pair) ->
    scml_base_list:car(caadr(Pair)).

-spec caadar(scm_pair()) -> scm_obj().
caadar(Pair) ->
    scml_base_list:car(cadar(Pair)).

-spec caaddr(scm_pair()) -> scm_obj().
caaddr(Pair) ->
    scml_base_list:car(caddr(Pair)).

-spec cadaar(scm_pair()) -> scm_obj().
cadaar(Pair) ->
    scml_base_list:car(cdaar(Pair)).

-spec cadadr(scm_pair()) -> scm_obj().
cadadr(Pair) ->
    scml_base_list:car(cdadr(Pair)).

-spec caddar(scm_pair()) -> scm_obj().
caddar(Pair) ->
    scml_base_list:car(cddar(Pair)).

-spec cadddr(scm_pair()) -> scm_obj().
cadddr(Pair) ->
    scml_base_list:car(cdddr(Pair)).

-spec cdaaar(scm_pair()) -> scm_obj().
cdaaar(Pair) ->
    scml_base_list:cdr(caaar(Pair)).

-spec cdaadr(scm_pair()) -> scm_obj().
cdaadr(Pair) ->
    scml_base_list:cdr(caadr(Pair)).

-spec cdadar(scm_pair()) -> scm_obj().
cdadar(Pair) ->
    scml_base_list:cdr(cadar(Pair)).

-spec cdaddr(scm_pair()) -> scm_obj().
cdaddr(Pair) ->
    scml_base_list:cdr(caddr(Pair)).

-spec cddaar(scm_pair()) -> scm_obj().
cddaar(Pair) ->
    scml_base_list:cdr(cdaar(Pair)).

-spec cddadr(scm_pair()) -> scm_obj().
cddadr(Pair) ->
    scml_base_list:cdr(cdadr(Pair)).

-spec cdddar(scm_pair()) -> scm_obj().
cdddar(Pair) ->
    scml_base_list:cdr(cddar(Pair)).

-spec cddddr(scm_pair()) -> scm_obj().
cddddr(Pair) ->
    scml_base_list:cdr(cdddr(Pair)).
