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
%%% An event manager responsible for distributing session-related events.
%%% Event handler must implement the {@link gen_event} behaviour and be ready
%%% to receive events on the `handle_event/2' callback.
%%% @end
%%%=============================================================================

-module(eipmi_events).

-export([start_link/0,
         add_handler/2,
         add_sup_handler/2,
         delete_handler/2,
         swap/4,
         fire/3,
         list_handlers/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start a locally registered event manager.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%%------------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link gen_event:add_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec add_handler(module() | {module(), term()}, term()) -> ok | {error, term()}.
add_handler(Handler, Args) ->
    case gen_event:add_handler(?MODULE, Handler, Args) of
        Reason = {'EXIT', _} ->
            {error, Reason};
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link gen_event:add_sup_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec add_sup_handler(module() | {module(), term()}, term()) ->
                             ok | {error, term()}.
add_sup_handler(Handler, Args) ->
    case gen_event:add_sup_handler(?MODULE, Handler, Args) of
        Reason = {'EXIT', _} ->
            {error, Reason};
        Other ->
            Other
    end.

%%------------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link gen_event:delete_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec delete_handler(module() | {module(), term()}, term()) -> term().
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

%%------------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link gen_event:swap_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec swap(module() | {module(), term()}, term(),
           module() | {module(), term()}, term()) ->
                  ok | {error, term()}.
swap(OldHandler, OldArgs, NewHandler, NewArgs) ->
    Old = {OldHandler, OldArgs},
    New = {NewHandler, NewArgs},
    gen_event:swap_handler(?MODULE, Old, New).

%%------------------------------------------------------------------------------
%% @doc
%% Sends event notifications to all registered event handlers. This is used by
%% {@link eipmi_session} to publish new events asynchronously.
%% @end
%%------------------------------------------------------------------------------
-spec fire(eipmi:session(), inet:ip_address() | inet:hostname(), term()) -> ok.
fire(Session, Address, Event) ->
    gen_event:notify(?MODULE, {ipmi, Session, Address, Event}).

%%------------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link gen_event:swap_handler/3}.
%% @end
%%------------------------------------------------------------------------------
-spec list_handlers() -> [module() | {module(), term()}].
list_handlers() ->
    gen_event:which_handlers(?MODULE).
