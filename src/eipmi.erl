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

-define(RMCP_PORT_NUMBER, 623).
-define(ANY_FREE_PORT_NUMBER, 0).

-define(RMCP_VERSION, 16#06).
-define(RMCP_RESERVED, 0).
-define(RMCP_CLASS, 16#06).
-define(RMCP_IANA, 35505). %% TODO: Configuration option
-define(RMCP_PING, 16#80).
-define(RMCP_PONG, 16#40).

-define(PING_MSG(Sequence, Tag), <<?RMCP_VERSION, ?RMCP_RESERVED, Sequence, ?RMCP_CLASS, ?RMCP_IANA:(4*8), ?RMCP_PING, Tag, 0, 0>>).
-define(PONG_MSG(Sequence, Tag), <<?RMCP_VERSION, ?RMCP_RESERVED, Sequence, ?RMCP_CLASS, ?RMCP_IANA:(4*8), ?RMCP_PONG, Tag, _Rest/binary>>).
-define(ACK_MSG(Sequence),       <<?RMCP_VERSION, ?RMCP_RESERVED, Sequence, (2#10000000 + ?RMCP_CLASS)>>).

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
    Sequence = 0,
    Tag = 11,
    Timeout = 1000,
    ok = gen_udp:send(Socket, Address, ?RMCP_PORT_NUMBER, ?PING_MSG(Sequence, Tag)),
    Result = receive
                 {udp, Socket, _, _, ?ACK_MSG(Sequence)} ->
                     receive
                         {udp, Socket, _, _, ?PONG_MSG(Sequence, Tag)} ->
                             pong
                     after Timeout ->
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

