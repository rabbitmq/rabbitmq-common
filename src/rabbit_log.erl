%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%

-module(rabbit_log).

-export([log/2, log/3, log/4]).
-export([debug/1, debug/2, debug/3,
         info/1, info/2, info/3,
         notice/1, notice/2, notice/3,
         warning/1, warning/2, warning/3,
         error/1, error/2, error/3,
         critical/1, critical/2, critical/3,
         alert/1, alert/2, alert/3,
         emergency/1, emergency/2, emergency/3,
         none/1, none/2, none/3]).
-export([make_internal_sink_name/1]).

-include("rabbit_log.hrl").
%%----------------------------------------------------------------------------

-type category() :: channel |
                    connection |
                    federation |
                    feature_flags |
                    ldap |
                    mirroring |
                    osiris |
                    prelaunch |
                    queue |
                    ra |
                    shovel |
                    upgrade.

-spec debug(string()) -> 'ok'.
-spec debug(string(), [any()]) -> 'ok'.
-spec debug(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec info(string()) -> 'ok'.
-spec info(string(), [any()]) -> 'ok'.
-spec info(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec notice(string()) -> 'ok'.
-spec notice(string(), [any()]) -> 'ok'.
-spec notice(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec warning(string()) -> 'ok'.
-spec warning(string(), [any()]) -> 'ok'.
-spec warning(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec error(string()) -> 'ok'.
-spec error(string(), [any()]) -> 'ok'.
-spec error(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec critical(string()) -> 'ok'.
-spec critical(string(), [any()]) -> 'ok'.
-spec critical(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec alert(string()) -> 'ok'.
-spec alert(string(), [any()]) -> 'ok'.
-spec alert(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec emergency(string()) -> 'ok'.
-spec emergency(string(), [any()]) -> 'ok'.
-spec emergency(pid() | [tuple()], string(), [any()]) -> 'ok'.
-spec none(string()) -> 'ok'.
-spec none(string(), [any()]) -> 'ok'.
-spec none(pid() | [tuple()], string(), [any()]) -> 'ok'.

%%----------------------------------------------------------------------------

-spec log(category(), lager:log_level(), string()) -> 'ok'.
log(Category, Level, Fmt) -> log(Category, Level, Fmt, []).

-spec log(category(), lager:log_level(), string(), [any()]) -> 'ok'.
log(Category, Level, Fmt, Args) when is_list(Args) ->
    Sink = case Category of
        default -> ?LAGER_SINK;
        _       -> make_internal_sink_name(Category)
    end,
    lager:log(Sink, Level, self(), Fmt, Args).

%% logger(3) handler.
log(#{level := Level,
      msg := Msg,
      meta := #{pid := Pid}} = _LogEvent,
    _Config) ->
    case Msg of
        {report, #{label := {error_logger, _}}} ->
            %% Avoid recursive loop.
            ok;
        {report, #{label := {application_controller, progress}}} ->
            %% Already logged by Lager.
            ok;
        {report, #{label := {supervisor, progress}}} ->
            %% Already logged by Lager.
            ok;
        {report, #{report := Report}} ->
            %% FIXME: Is this code reached?
            error_logger:info_report(Report);
        {report, #{format := Format, args := Args}} when is_list(Format) ->
            lager:log(?LAGER_SINK, Level, Pid, Format, Args);
        {string, String} ->
            lager:log(?LAGER_SINK, Level, Pid, "~ts", [String]);
        {Format, Args} when is_list(Format) ->
            lager:log(?LAGER_SINK, Level, Pid, Format, Args)
    end.

make_internal_sink_name(channel)    -> rabbit_log_channel_lager_event;
make_internal_sink_name(connection) -> rabbit_log_connection_lager_event;
make_internal_sink_name(default)    -> rabbit_log_lager_event;
make_internal_sink_name(feature_flags) -> rabbit_log_feature_flags_lager_event;
make_internal_sink_name(federation) -> rabbit_log_federation_lager_event;
make_internal_sink_name(ldap)       -> rabbit_log_ldap_lager_event;
make_internal_sink_name(mirroring)  -> rabbit_log_mirroring_lager_event;
make_internal_sink_name(osiris)     -> rabbit_log_osiris_lager_event;
make_internal_sink_name(prelaunch)  -> rabbit_log_prelaunch_lager_event;
make_internal_sink_name(queue)      -> rabbit_log_queue_lager_event;
make_internal_sink_name(ra)         -> rabbit_log_ra_lager_event;
make_internal_sink_name(shovel)     -> rabbit_log_shovel_lager_event;
make_internal_sink_name(upgrade)    -> rabbit_log_upgrade_lager_event;
make_internal_sink_name(Category)   ->
    erlang:error({unknown_category, Category}).

debug(Format) -> debug(Format, []).
debug(Format, Args) -> debug(self(), Format, Args).
debug(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, debug, Metadata, Format, Args).

info(Format) -> info(Format, []).
info(Format, Args) -> info(self(), Format, Args).
info(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, info, Metadata, Format, Args).

notice(Format) -> notice(Format, []).
notice(Format, Args) -> notice(self(), Format, Args).
notice(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, notice, Metadata, Format, Args).

warning(Format) -> warning(Format, []).
warning(Format, Args) -> warning(self(), Format, Args).
warning(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, warning, Metadata, Format, Args).

error(Format) -> ?MODULE:error(Format, []).
error(Format, Args) -> ?MODULE:error(self(), Format, Args).
error(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, error, Metadata, Format, Args).

critical(Format) -> critical(Format, []).
critical(Format, Args) -> critical(self(), Format, Args).
critical(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, critical, Metadata, Format, Args).

alert(Format) -> alert(Format, []).
alert(Format, Args) -> alert(self(), Format, Args).
alert(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, alert, Metadata, Format, Args).

emergency(Format) -> emergency(Format, []).
emergency(Format, Args) -> emergency(self(), Format, Args).
emergency(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, emergency, Metadata, Format, Args).

none(Format) -> none(Format, []).
none(Format, Args) -> none(self(), Format, Args).
none(Metadata, Format, Args) ->
    lager:log(?LAGER_SINK, none, Metadata, Format, Args).
