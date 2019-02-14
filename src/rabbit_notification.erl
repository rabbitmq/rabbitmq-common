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
%% Copyright (c) 2007-2019 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_notification).

-export([notify/2]).
-export([source/2]).

%%----------------------------------------------------------------------------

-type notification_client() :: rabbit_types:proc_name() |
    rabbit_types:proc_type_and_name().

-spec notify(notification_client(), any()) -> any().
-spec source(notification_client(), any()) -> any().

%%----------------------------------------------------------------------------

notify(ClientPid, Notification) when is_pid(ClientPid) ->
    case erlang:is_process_alive(ClientPid) of
        true  -> ClientPid ! Notification;
        false -> ok
    end;
notify(ClientName, Source) when is_atom(ClientName) ->
    case catch rabbit_misc:name_to_pid(ClientName) of
        Pid when is_pid(Pid) -> notify(Pid, Source);
        {'EXIT', Error}      -> {error, Error};
        Error                -> {error, Error}
    end.

source(Client, Source) ->
    notify(Client, {source_notification, Source}).
