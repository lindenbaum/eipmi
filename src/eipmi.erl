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
         read_fru_inventory/2,
         read_sdr/2,
         read_sdr_repository/1,
         read_sel/2,
         raw/4,
         sel_to_sdr/2,
         sel_to_fru/3,
         sdr_to_fru/3,
         subscribe/2,
         unsubscribe/2,
         stats/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

%% supervisor callbacks
-export([init/1]).

-registered([?MODULE]).

-include("eipmi.hrl").

-type target() :: {inet:ip_address() | inet:hostname(), inet:port_number()}.

-opaque session() :: {target(), reference()}.

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

-export_type([target/0,
              session/0,
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
%% The returned handle can be used to send requests to the target BMC using one
%% of the functions provided by this module (e.g. {@link raw/4}) or close the
%% session using {@link close/1}.
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
%%     the requested privilege level for this session, default is
%%     `administrator'
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
    Target = {IPAddress, eipmi_util:get_val(port, Options, ?RMCP_PORT_NUMBER)},
    Session = {Target, erlang:make_ref()},
    Start = {eipmi_session, start_link, [Session, IPAddress, Options]},
    Spec = {Session, Start, temporary, 2000, worker, [eipmi_session]},
    case supervisor:start_child(?MODULE, Spec) of
        {ok, _} ->
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
%% Return the FRU inventory data for a specific FRU id. The returned FRU
%% information is a property list that does only contain the available and
%% checksum error free fields of the inventory. If no FRU data is available for
%% a specific id the returned inventory data is the empty list.
%% @end
%%------------------------------------------------------------------------------
-spec read_fru(session(), 0..254) ->
                      {ok, eipmi_fru:info()} | {error, term()}.
read_fru(Session = {_, _}, FruId) when FruId >= 0 andalso FruId < 255 ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            ?EIPMI_CATCH(eipmi_fru:read(Pid, FruId));
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return the FRU inventory data for all FRU represented by a FRU Device Locator
%% Record in the Sensor Data Record (SDR) Repository.
%% @see read_fru/2
%% @end
%%------------------------------------------------------------------------------
-spec read_fru_inventory(session(), [eipmi_sdr:entry()]) ->
                                {ok, [eipmi_fru:info()]} | {error, [term()]}.
read_fru_inventory(Session, SdrRepository) ->
    collect([read_fru(Session, FruId) || FruId <- get_fru_ids(SdrRepository)]).

%%------------------------------------------------------------------------------
%% @doc
%% Return a specific record from the Sensor Data Record (SDR) Repository.
%%
%% Reading of the SDR may fail (e.g. return `{error, term()}') when the
%% reservation for SDR reading with non-zero offsets gets cancelled. This is not
%% a severe error. It is most likely that the SDR can be read successfully when
%% retried.
%% @end
%%------------------------------------------------------------------------------
-spec read_sdr(session(), non_neg_integer()) ->
                      {ok, eipmi_sdr:entry()} | {error, term()}.
read_sdr(Session = {_, _}, RecordId) ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            case ?EIPMI_CATCH(eipmi_sdr:read(Pid, RecordId)) of
                Error = {error, _} ->
                    Error;
                Entries ->
                    {ok, Entries}
            end;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return all records contained in the Sensor Data Record (SDR) Repository.
%%
%% Reading of the SDR may fail (e.g. return `{error, term()}') when the
%% reservation for SDR reading with non-zero offsets gets cancelled. This is not
%% a severe error. It is most likely that the SDR can be read successfully when
%% retried.
%% @end
%%------------------------------------------------------------------------------
-spec read_sdr_repository(session()) ->
                                 {ok, [eipmi_sdr:entry()]} | {error, term()}.
read_sdr_repository(Session = {_, _}) ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            case ?EIPMI_CATCH(eipmi_sdr:read(Pid)) of
                Error = {error, _} ->
                    Error;
                Entries ->
                    {ok, Entries}
            end;
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return all currently available entries from the System Event Log (SEL). The
%% returned SEL entries are property lists that do only contain the available
%% and checksum error free fields of the respective entry. Using the second
%% argument the SEL can optionally be cleared after reading.
%%
%% Clearing the SEL may fail in rare cases when the reservation for SEL
%% clearance gets cancelled by the BMC. This will be ignored. However in this
%% case the following read may return duplicates of already read events.
%% @end
%%------------------------------------------------------------------------------
-spec read_sel(session(), boolean()) ->
                      {ok, [eipmi_sel:entry()]} | {error, term()}.
read_sel(Session = {_, _}, Clear) ->
    case get_session(Session, supervisor:which_children(?MODULE)) of
        {ok, Pid} ->
            case ?EIPMI_CATCH(eipmi_sel:read(Pid, Clear)) of
                Error = {error, _} ->
                    Error;
                Entries ->
                    {ok, Entries}
            end;
        Error ->
            Error
    end.

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
            eipmi_session:rpc(Pid, {NetFn, Command}, Properties);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the Sensor Data Record (SDR) from the SDR repository associated with
%% an entry in the System Event Log (SEL), if any.
%% @end
%%------------------------------------------------------------------------------
-spec sel_to_sdr(eipmi_sel:entry(), [eipmi_sdr:entry()]) ->
                        {ok, eipmi_sdr:entry()} | {error, term()}.
sel_to_sdr({_, SelEntryProps}, SdrRepository) ->
    get_element_by_properties(
      maybe_keyfind(sensor_number, 1, SelEntryProps)
      ++ maybe_keyfind(sensor_addr, 1, SelEntryProps)
      ++ maybe_keyfind(software_id, 1, SelEntryProps),
      SdrRepository).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU inventory data associated with an entry in the System
%% Event Log (SEL), if any.
%% @end
%%------------------------------------------------------------------------------
-spec sel_to_fru(eipmi_sel:entry(), [eipmi_sdr:entry()], [eipmi_fru:info()]) ->
                        {ok, eipmi_fru:info()} | {error, term()}.
sel_to_fru(SelEntry, SdrRepository, FruInventory) ->
    case sel_to_sdr(SelEntry, SdrRepository) of
        {ok, Sdr} ->
            sdr_to_fru(Sdr, SdrRepository, FruInventory);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Returns the FRU inventory data associated with a specific sensor from the
%% Sensor Data Record (SDR) repository, if any.
%% @end
%%------------------------------------------------------------------------------
-spec sdr_to_fru(eipmi_sdr:entry(), [eipmi_sdr:entry()], [eipmi_fru:info()]) ->
                        {ok, eipmi_fru:info()} | {error, term()}.
sdr_to_fru({_, SensorProps}, SdrRepository, FruInventory) ->
    sdr_to_fru(
      FruInventory,
      get_element_by_properties(
        maybe_keyfind(entity_id, 1, SensorProps)
        ++ maybe_keyfind(entity_instance, 1, SensorProps),
        filter_by_key(fru_device_locator, SdrRepository))).
sdr_to_fru(FruInventory, {ok, {fru_device_locator, Props}}) ->
    get_element_by_properties(maybe_keyfind(fru_id, 1, Props), FruInventory);
sdr_to_fru(_FruInventory, Error = {error, _}) ->
    Error.

%%------------------------------------------------------------------------------
%% @doc
%% Registers/adds a handler for session related events. The handler module must
%% implement the {@link gen_event} behaviour. For more information on the
%% arguments `Handler' and `Args' refer to {@link gen_event:add_handler/3}.
%% The event handling module should be prepared to receive the following events
%% on the `handle_call/2' callback:
%% <dl>
%%   <dt>`{Session :: session(), established}'</dt>
%%   <dd>
%%     <p>the session was successfully established and activated</p>
%%   </dd>
%%   <dt>`{Session :: session(), {closed, Reason :: term()}}'</dt>
%%   <dd>
%%     <p>the session was closed with the provided reason</p>
%%   </dd>
%%   <dt>`{Session :: session(), {decode_error, Reason :: term()}}'</dt>
%%   <dd>
%%     <p>a received packet could not be decoded</p>
%%   </dd>
%%   <dt>`{Session :: session(), {request_timeout,
%%                                RqSeqNr :: non_neg_integer()}}'</dt>
%%   <dd>
%%     <p>the corresponding request timed out</p>
%%   </dd>
%%   <dt>`{Session :: session(), {no_requestor, {RqSeqNr :: non_neg_integer(),
%%                                               Response}}}'</dt>
%%   <dd>
%%     <p>no requestor could be found for the corresponding request</p>
%%   </dd>
%% </dl>
%% @end
%%------------------------------------------------------------------------------
-spec subscribe(module() | {module(), term()}, term()) ->
                       ok | {error, term()}.
subscribe(Handler, Args) ->
    eipmi_events:subscribe(Handler, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Unregisters/removes a handler for session related events previously added
%% using {@link subscribe/2}. For more information refer to on the arguments
%% {@link gen_event:delete_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec unsubscribe(module() | {module(), term()}, term()) ->
                         term().
unsubscribe(Handler, Args) ->
    eipmi_events:unsubscribe(Handler, Args).

%%------------------------------------------------------------------------------
%% @doc
%% Returns statistical information about the currently opened sessions and the
%% registered event handlers.
%% @end
%%------------------------------------------------------------------------------
-spec stats() ->
                   [{sessions, [{target(), pid()}]} |
                    {handlers, [module() | {module(), term()}]}].
stats() ->
    Children = supervisor:which_children(?MODULE),
    [{sessions, get_sessions(Children)},
     {handlers, eipmi_events:list_handlers()}].

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
    {ok, {{one_for_one, 0, 1},
          [{eipmi_events,
            {eipmi_events, start_link, []},
            permanent, 2000, worker, dynamic}]}}.

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
get_sessions(Cs) ->
    [{Target, P} || {{Target, _Ref}, P, worker, _} <- Cs, is_pid(P)].

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
get_fru_ids(SdrRepository) ->
    FruRecords = filter_by_key(fru_device_locator, SdrRepository),
    [FruId || FruId <- [eipmi_util:get_val(fru_id, Ps) || {_, Ps} <- FruRecords],
              FruId =/= undefined].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_element_by_properties([], _List) ->
    {error, unresolvable};
get_element_by_properties(Props, List) ->
    Pred = fun(Ps) -> lists:all(fun(P) -> lists:member(P, Ps) end, Props) end,
    case [Element || Element = {_, Ps} <- List, Pred(Ps)] of
        [] ->
            {error, {not_found, Props}};
        [Element | _] ->
            {ok, Element}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
filter_by_key(Key, List) ->
    [Element || Element = {K, _} <- List, K =:= Key].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
collect(Results) ->
    lists:foldl(
      fun({error, Error}, {ok, _}) ->
              {error, [Error]};
         ({error, Error}, {error, Errors}) ->
              {error, [Error | Errors]};
         ({ok, _}, Error = {error, _}) ->
              Error;
         ({ok, Ok}, {ok, Oks}) ->
              {ok, [Ok | Oks]}
      end,
      {ok, []},
      Results).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_keyfind(Key, N, List) ->
    maybe_keyfind(lists:keyfind(Key, N, List)).
maybe_keyfind(false) ->
    [];
maybe_keyfind(Otherwise) ->
    [Otherwise].
