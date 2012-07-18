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

%% Public API
-export([ping/1]).

-include("eipmi.hrl").

-define(RMCP_PORT_NUMBER, 623).
-define(ANY_FREE_PORT_NUMBER, 0).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% TODO
%% @end
%%------------------------------------------------------------------------------
ping(Address) ->
    {ok, Socket} = gen_udp:open(?ANY_FREE_PORT_NUMBER, [binary]),
    Timeout = 1000,
    {ok, Ping} = eipmi_messages:encode(#rmcp_ping{seq_nr = 0, asf_tag = 0}),
    ok = gen_udp:send(Socket, Address, ?RMCP_PORT_NUMBER, Ping),
    Result = receive
                 {udp, Socket, _, _, Bin1} ->
                     case eipmi_messages:decode(Bin1) of
                         {ok, #rmcp_ack{}} ->
                             receive
                                 {udp, Socket, _, _, Bin2} ->
                                     case eipmi_messages:decode(Bin2) of
                                         {ok, #rmcp_pong{}} ->
                                             pong;
                                         _ ->
                                             pang
                                     end
                             after Timeout ->
                                     pang
                             end;
                         _ ->
                             pang
                     end
             after Timeout ->
                     pang
             end,
    ok = gen_udp:close(Socket),
    Result.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

