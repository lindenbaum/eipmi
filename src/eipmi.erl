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
%%% The main entry module of the `eipmi' application containing the application
%%% callback as well as the top level supervisor.
%%%
%%% This module provides capabilities to discover and use IPMI-capable devices.
%%% The {@link eipmi_session} module provides a IPMI session implementation that
%%% is able to send requests and receive responses implemented in the
%%% {@link eipmi_request} and {@link eipmi_response} modules. Frontend API
%%% functions using a combination of several requests to provide a certain
%%% feature should be put here.
%%% @end
%%%=============================================================================
-module(eipmi).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([ping/1,
         ping/2,
         open/1,
         open/2,
         close/1,
         read_fru/2,
         raw/4]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% supervisor callbacks
-export([init/1]).

-registered([?MODULE]).

-include("eipmi.hrl").

-opaque session() :: {inet:ip_address() | inet:hostname(), reference()}.

-type req_net_fn()  :: 16#04 | 16#06 | 16#a | 16#c.
-type resp_net_fn() :: 16#05 | 16#07 | 16#b | 16#d.

-type request() :: {req_net_fn(), Command :: 0..255}.
-type response() :: {resp_net_fn(), Command :: 0..255}.

-type option() ::
        {initial_outbound_seq_nr, non_neg_integer()} |
        {password, string()} |
        {port, inet:port_number()} |
        {privilege, callback | user | operator | administrator} |
        {rq_addr, 16#81..16#8d} |
        {timeout, non_neg_integer()} |
        {user, string()}.

-type option_name() ::
        initial_outbound_seq_nr |
        password |
        port |
        privilege |
        rq_addr |
        timeout |
        user.

-export_type([session/0,
              req_net_fn/0,
              request/0,
              response/0,
              option/0,
              option_name/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Pings a given host using RMCP ping. This can be done to check a device for
%% the IPMI capability before opening a session to it. Default receive timeout
%% is 5000 milliseconds. Returns `pong' if the pinged host supports IPMI,
%% `pang' otherwise.
%% @see ping/2
%% @end
%%------------------------------------------------------------------------------
-spec ping(inet:ip_address() | inet:hostname()) ->
                  pang | pong.
ping(IPAddress) ->
    ping(IPAddress, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Same as {@link ping/1} but allows the specification of a custom receive
%% timeout value in milliseconds.
%% @end
%%------------------------------------------------------------------------------
-spec ping(inet:ip_address() | inet:hostname(), timeout()) ->
                  pang | pong.
ping(IPAddress, Timeout) when is_integer(Timeout) andalso Timeout > 0 ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    try do_ping(IPAddress, Timeout, Socket) of
        true -> pong;
        false -> pang
    catch
        _:_ -> pang
    after
        gen_udp:close(Socket)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Opens an IPMI session to a given host. With default parameters. Please note
%% that this will only work for IPMI targets supporting anonymous login. For all
%% other login types at least the `password' and maybe the `user' option will be
%% required.
%%
%% The returned handle can be used to send requests to the target BMC using TODO
%% or close the session using {@link close/1}.
%% @see open/2
%% @see close/1
%% @end
%%------------------------------------------------------------------------------
-spec open(inet:ip_address() | inet:hostname()) ->
                  {ok, session()} | {error, term()}.
open(IPAddress) ->
    open(IPAddress, []).

%%------------------------------------------------------------------------------
%% @doc
%% Same as {@link open/1} but allows the specification of the following custom
%% options:
%% <dl>
%%   <dt>`{initial_outbound_seq_nr, non_neg_integer()}'</dt>
%%   <dd>
%%     <p>
%%     the initial outbound sequence number that will be requested on the BMC,
%%     default is `16#1337'
%%     </p>
%%  </dd>
%%   <dt>`{password, string() with length <= 16bytes}'</dt>
%%   <dd>
%%     <p>
%%     a password string used for authentication when anonymous login is not
%%     available, default is `""'
%%     </p>
%%   </dd>
%%   <dt>`{port, inet:port_number()}'</dt>
%%   <dd>
%%     <p>
%%     the RMCP port the far end is expecting incoming RMCP and IPMI packets,
%%     default is `623'
%%     </p>
%%   </dd>
%%   <dt>`{privilege, callback | user | operator | administrator}'</dt>
%%   <dd>
%%     <p>
%%     the requested privilege level for this session, default is `administrator'
%%     </p>
%%   </dd>
%%   <dt>`{rq_addr, 16#81..16#8d}'</dt>
%%   <dd>
%%     <p>
%%     the requestor address used in IPMI lan packages, the default value of
%%     `16#81' should be suitable for all common cases
%%     </p>
%%   </dd>
%%   <dt>`{timeout, non_neg_integer()}'</dt>
%%   <dd>
%%     <p>
%%     the timeout for IPMI requests, default is `1000ms'
%%     </p>
%%   </dd>
%%   <dt>`{user, string() with length <= 16bytes}'</dt>
%%   <dd>
%%     <p>
%%     a user name string used for authentication when neither anonymous nor
%%     null user login are available, default is `""'
%%     </p>
%%   </dd>
%% </dl>
%% @end
%%------------------------------------------------------------------------------
-spec open(inet:ip_address() | inet:hostname(), [option()]) ->
                  {ok, session()} | {error, term()}.
open(IPAddress, Options) ->
    Session = {IPAddress, proplists:get_value(port, Options, ?RMCP_PORT_NUMBER)},
    Start = {eipmi_session, start_link, [Session, IPAddress, Options]},
    Spec = {Session, Start, temporary, 2000, worker, [eipmi_session]},
    case supervisor:start_child(?MODULE, Spec) of
        {ok, _} ->
            {ok, Session};
        {error, {already_started, _}} ->
            {ok, Session};
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Closes an IPMI session previously opened with {@link open/1} or
%% {@link open/2}. This will return `{error, no_session}' when the given
%% session is not active any more.
%% @end
%%------------------------------------------------------------------------------
-spec close(session()) ->
                   ok | {error, no_session}.
close(Session = {_, _}) ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            eipmi_session:stop(Pid);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns all data from the FRU inventory info area for a given FRU id.
%% @end
%%------------------------------------------------------------------------------
-spec read_fru(session(), 0..254) ->
                      {ok, binary()} | {error, term()}.
read_fru(Session, FruId) when FruId >= 0 andalso FruId < 255 ->
    do_read_fru(get_session(Session, supervisor:which_children(?MODULE)), FruId).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a raw IPMI command over a given session. DO NOT USE THIS unless you
%% really know what you're doing!
%% @end
%%------------------------------------------------------------------------------
-spec raw(session(), req_net_fn(), 0..255, proplists:proplist()) ->
                 {ok, proplists:proplist()} | {error, term()}.
raw(Session = {_, _}, NetFn, Command, Properties) ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            eipmi_session:request(Pid, {NetFn, Command}, Properties);
        Error ->
            Error
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
    {ok, {{one_for_one, 0, 1}, []}}.

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_session(S, Cs) ->
    case [P || {I, P, worker, _} <- Cs, I =:= S andalso is_pid(P)] of
        [] ->
            {error, no_session};
        [P] ->
            {ok, P}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping(IPAddress, Timeout, Socket) ->
    Ping = eipmi_encoder:ping(#rmcp_header{seq_nr = 0}, #asf_ping{}),
    ok = gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Ping),
    do_ping_receive(IPAddress, Timeout, Socket).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_ping_receive(IPAddress, Timeout, Socket) ->
    {ok, {_, _, Packet}} = gen_udp:recv(Socket, 8192, Timeout),
    case eipmi_decoder:packet(Packet) of
        {ok, #rmcp_ack{}} ->
            do_ping_receive(IPAddress, Timeout, Socket);
        {ok, #rmcp_asf{header = H, payload = #asf_pong{entities = Es}}} ->
            Ack = eipmi_encoder:ack(H),
            gen_udp:send(Socket, IPAddress, ?RMCP_PORT_NUMBER, Ack),
            Es =:= [ipmi]
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_read_fru({ok, Pid}, FruId) ->
    GetFruInfo = {?IPMI_NETFN_STORAGE_REQUEST, ?GET_FRU_INVENTORY_AREA_INFO},
    {ok, Response} = eipmi_session:request(Pid, GetFruInfo, [{fru_id, FruId}]),
    AreaSize = eipmi_util:get_val(area_size, Response),
    {ok, do_read_fru(FruId, Pid, AreaSize, {0, <<>>})};
do_read_fru(Error, _FruId) ->
    Error.
do_read_fru(_FruId, _Pid, Size, {Size, Acc}) ->
    Acc;
do_read_fru(FruId, Pid, Size, {Offset, Acc}) when (Offset + 32) =< Size ->
    Count = 32,
    do_read_fru(FruId, Pid, Size, do_read_fru(FruId, Pid, Offset, Count, Acc));
do_read_fru(FruId, Pid, Size, {Offset, Acc}) ->
    Count = Size - Offset,
    do_read_fru(FruId, Pid, Size, do_read_fru(FruId, Pid, Offset, Count, Acc)).
do_read_fru(FruId, Pid, Offset, Count, Acc) ->
    ReadFru = {?IPMI_NETFN_STORAGE_REQUEST, ?READ_FRU_DATA},
    Properties = [{fru_id, FruId}, {offset, Offset}, {count, Count}],
    {ok, Response} = eipmi_session:request(Pid, ReadFru, Properties),
    {Offset + eipmi_util:get_val(count, Response),
     <<Acc/binary, (eipmi_util:get_val(data, Response))/binary>>}.
