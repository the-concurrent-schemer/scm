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

%%% @doc Scheme read library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_read).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['read'/0
         , 'read'/1
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
    [{'read', #nipn{val=[fun ?MODULE:'read'/0, fun ?MODULE:'read'/1]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'read'() -> scm_obj() | scm_eof().
'read'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'read'(scm_port()) -> scm_obj() | scm_eof().
'read'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, [Port]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
