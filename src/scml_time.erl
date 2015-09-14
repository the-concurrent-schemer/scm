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

%%% @doc Scheme time library
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(scml_time).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['current-second'/0
         , 'current-jiffy'/0
         , 'jiffies-per-second'/0
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
    [{'current-second', #nip0{val=fun ?MODULE:'current-second'/0}}
     , {'current-jiffy', #nip0{val=fun ?MODULE:'current-jiffy'/0}}
     , {'jiffies-per-second', #nip0{val=fun ?MODULE:'jiffies-per-second'/0}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'current-second'() -> scm_x().
'current-second'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'current-jiffy'() -> scm_k().
'current-jiffy'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

-spec 'jiffies-per-second'() -> scm_k().
'jiffies-per-second'() ->
    %% @TODO
    erlang:error({roadmap,'v0.6.0'}, []).

%%%===================================================================
%%% internal helpers
%%%===================================================================
