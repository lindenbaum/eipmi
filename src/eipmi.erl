%%%=============================================================================
%%% Copyright (c) 2012 Lindenbaum GmbH
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% TODO
%%% @end
%%%=============================================================================
-module(eipmi).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([initiate/1,
         ping/1]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% supervisor callbacks
-export([init/1]).

-include("eipmi.hrl").

-registered([?MODULE]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
initiate(IPAddress) ->
    supervisor:start_child(?MODULE, [IPAddress]).

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
ping(IPAddress) ->
    Timeout = 1000,
    {ok, Session} = initiate(IPAddress),
    Monitor = erlang:monitor(process, Session),
    receive
        {'DOWN', Monitor, process, Session, normal} ->
            pong;

        _ ->
            pang
    after Timeout ->
            pang
    end.

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
stop(_State) ->
    ok.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    Spec = {eipmi_session, {eipmi_session, start_link, []},
            temporary, 2000, worker, [eipmi_session]},
    {ok, {{simple_one_for_one, 0, 1}, [Spec]}}.
