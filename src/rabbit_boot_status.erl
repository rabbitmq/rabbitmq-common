%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2017 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_boot_status).

-export([await/0, await/1,
         is_running/0, is_running/1,
         wait_for_finish/1]).

%% TODO this only determines if the rabbit application has started,
%% not if it is running, never mind plugins. It would be nice to have
%% more nuance here.
-spec is_running() -> boolean().
is_running() ->
    is_running(node()).

-spec is_running(node()) -> boolean().
is_running(Node) ->
    rabbit_nodes_common:is_process_running(Node, rabbit).

-spec await() -> 'ok'.
await() ->
    await(node()).

-spec await(node()) -> 'ok'.
await(Node) ->
    case is_booting(Node) of
        true -> wait_for_finish(Node);
        false ->
            case is_running(Node) of
                true -> ok;
                false -> wait_for_start(Node),
                         wait_for_finish(Node)
            end
    end.

is_booting(Node) ->
    case rpc:call(Node, erlang, whereis, [rabbit_boot]) of
        {badrpc, _} = Err -> Err;
        undefined         -> false;
        P when is_pid(P)  -> true
    end.

wait_for_start(Node) ->
    case is_booting(Node) of
        false ->
            timer:sleep(100),
            wait_for_start(Node);
        {badrpc, _} = Err ->
            Err;
        true  ->
            ok
    end.

wait_for_finish(Node) ->
    case is_booting(Node) of
        false ->
            %% We don't want badrpc error to be interpreted as false,
            %% so we don't call rabbit:is_running(Node)
            case rpc:call(Node, rabbit, is_running, []) of
                true              -> ok;
                false             -> {error, rabbit_is_not_running};
                {badrpc, _} = Err -> Err
            end;
        {badrpc, _} = Err ->
            Err;
        true  ->
            timer:sleep(100),
            wait_for_finish(Node)
    end.
