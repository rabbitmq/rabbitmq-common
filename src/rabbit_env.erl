%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2019 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_env).

-include_lib("kernel/include/file.hrl").

-export([get_context/0,
         get_context/1,
         get_context_before_logging_init/0,
         get_context_before_logging_init/1,
         get_context_after_logging_init/1,
         get_context_after_reloading_env/1,
         dbg_config/0,
         log_process_env/0,
         log_context/1,
         context_to_app_env_vars/1,
         context_to_app_env_vars_no_logging/1,
         context_to_code_path/1]).

-ifdef(TEST).
-export([value_is_yes/1]).
-endif.

-define(USED_ENV_VARS,
        [
         "RABBITMQ_ALLOW_INPUT",
         "RABBITMQ_ADVANCED_CONFIG_FILE",
         "RABBITMQ_BASE",
         "RABBITMQ_CONF_ENV_FILE",
         "RABBITMQ_CONFIG_FILE",
         "RABBITMQ_DBG",
         "RABBITMQ_DIST_PORT",
         "RABBITMQ_ENABLED_PLUGINS",
         "RABBITMQ_ENABLED_PLUGINS_FILE",
         "RABBITMQ_FEATURE_FLAGS_FILE",
         "RABBITMQ_HOME",
         "RABBITMQ_KEEP_PID_FILE_ON_EXIT",
         "RABBITMQ_LOG",
         "RABBITMQ_LOG_BASE",
         "RABBITMQ_LOGS",
         "RABBITMQ_MNESIA_BASE",
         "RABBITMQ_MNESIA_DIR",
         "RABBITMQ_NODE_IP_ADDRESS",
         "RABBITMQ_NODE_PORT",
         "RABBITMQ_NODENAME",
         "RABBITMQ_PID_FILE",
         "RABBITMQ_PLUGINS_DIR",
         "RABBITMQ_PLUGINS_EXPAND_DIR",
         "RABBITMQ_QUORUM_DIR",
         "RABBITMQ_UPGRADE_LOG",
         "RABBITMQ_USE_LONGNAME",
         "SYS_PREFIX"
        ]).

get_context() ->
    Context0 = get_context_before_logging_init(),
    Context1 = get_context_after_logging_init(Context0),
    get_context_after_reloading_env(Context1).

get_context(TakeFromRemoteNode) ->
    Context0 = get_context_before_logging_init(TakeFromRemoteNode),
    Context1 = get_context_after_logging_init(Context0),
    get_context_after_reloading_env(Context1).

get_context_before_logging_init() ->
    get_context_before_logging_init(false).

get_context_before_logging_init(TakeFromRemoteNode) ->
    %% The order of steps below is important because some of them
    %% depends on previous steps.
    Steps = [
             fun log_levels/1,
             fun shell/1
            ],

    run_context_steps(context_base(TakeFromRemoteNode), Steps).

get_context_after_logging_init(Context) ->
    %% The order of steps below is important because some of them
    %% depends on previous steps.
    Steps = [
             fun data_dir/1,
             fun config_base_dir/1,
             fun load_conf_env_file/1,
             fun log_levels/1
            ],

    run_context_steps(Context, Steps).

get_context_after_reloading_env(Context) ->
    %% The order of steps below is important because some of them
    %% depends on previous steps.
    Steps = [
             fun node_name_and_type/1,
             fun maybe_setup_dist_for_remote_query/1,
             fun dbg_config/1,
             fun config_files/1,
             fun log_files/1,
             fun mnesia_dir/1,
             fun quorum_dir/1,
             fun pid_file/1,
             fun feature_flags_file/1,
             fun plugins_dirs/1,
             fun maybe_stop_dist_for_remote_query/1,
             fun tcp_configuration/1
            ],

    run_context_steps(Context, Steps).

context_base(TakeFromRemoteNode) ->
    OSType = get_os_type(),
    Context = #{os_type => OSType},
    case TakeFromRemoteNode of
        false ->
            Context;
        offline ->
            Context#{from_remote_node => TakeFromRemoteNode};
        _ when is_atom(TakeFromRemoteNode) ->
            Context#{from_remote_node => {TakeFromRemoteNode, 10000}};
        {RemoteNode, infinity}
          when is_atom(RemoteNode) ->
            Context#{from_remote_node => TakeFromRemoteNode};
        {RemoteNode, Timeout}
          when is_atom(RemoteNode) andalso
               is_integer(Timeout) andalso
               Timeout >= 0 ->
            Context#{from_remote_node => TakeFromRemoteNode}
    end.

-ifdef(TEST).
get_os_type() -> persistent_term:get({?MODULE, os_type}, os:type()).
-else.
get_os_type() -> os:type().
-endif.

run_context_steps(Context, Steps) ->
    lists:foldl(
      fun(Step, Context1) -> Step(Context1) end,
      Context,
      Steps).

log_process_env() ->
    rabbit_log_prelaunch:debug("Process environment:"),
    lists:foreach(
      fun({Var, Value}) ->
              rabbit_log_prelaunch:debug("  - ~s = ~ts", [Var, Value])
      end, lists:sort(os:list_env_vars())).

log_context(Context) ->
    rabbit_log_prelaunch:debug("Context (based on environment variables):"),
    lists:foreach(
      fun(Key) ->
              Value = maps:get(Key, Context),
              rabbit_log_prelaunch:debug("  - ~s: ~p", [Key, Value])
      end,
      lists:sort(maps:keys(Context))).

context_to_app_env_vars(Context) ->
    rabbit_log_prelaunch:debug(
      "Setting default application environment variables:"),
    Fun = fun({App, Param, Value}) ->
                  rabbit_log_prelaunch:debug(
                    "  - ~s:~s = ~p", [App, Param, Value]),
                  ok = application:set_env(
                         App, Param, Value, [{persistent, true}])
          end,
    context_to_app_env_vars1(Context, Fun).

context_to_app_env_vars_no_logging(Context) ->
    Fun = fun({App, Param, Value}) ->
                  ok = application:set_env(
                         App, Param, Value, [{persistent, true}])
          end,
    context_to_app_env_vars1(Context, Fun).

context_to_app_env_vars1(
  #{mnesia_dir := MnesiaDir,
    feature_flags_file := FFFile,
    quorum_queue_dir := QuorumQueueDir,
    plugins_path := PluginsPath,
    plugins_expand_dir := PluginsExpandDir,
    enabled_plugins_file := EnabledPluginsFile} = Context,
  Fun) ->
    lists:foreach(
      Fun,
      %% Those are all the application environment variables which
      %% were historically set on the erl(1) command line in
      %% rabbitmq-server(8).
      [{kernel, inet_default_connect_options, [{nodelay, true}]},
       {sasl, errlog_type, error},
       {os_mon, start_cpu_sup, false},
       {os_mon, start_disksup, false},
       {os_mon, start_memsup, false},
       {mnesia, dir, MnesiaDir},
       {ra, data_dir, QuorumQueueDir},
       {rabbit, feature_flags_file, FFFile},
       {rabbit, plugins_dir, PluginsPath},
       {rabbit, plugins_expand_dir, PluginsExpandDir},
       {rabbit, enabled_plugins_file, EnabledPluginsFile}]),

    case Context of
        #{erlang_dist_tcp_port := DistTcpPort} ->
            lists:foreach(
              Fun,
              [{kernel, inet_dist_listen_min, DistTcpPort},
               {kernel, inet_dist_listen_max, DistTcpPort}]);
        _ ->
            ok
    end,
    case Context of
        #{amqp_ipaddr_and_tcp_port := {IpAddr, TcpPort}}
          when IpAddr /= undefined andalso TcpPort /= undefined ->
            Fun({rabbit, tcp_listeners, [{IpAddr, TcpPort}]});
        _ ->
            ok
    end,
    ok.

context_to_code_path(#{plugins_path := PluginsPath}) ->
    Dirs = get_user_lib_dirs(PluginsPath),
    add_paths_to_code_path(Dirs).

add_paths_to_code_path(Dirs) ->
    Path = code:get_path(),
    add_paths_to_code_path1(Path, lists:reverse(Dirs)).

add_paths_to_code_path1(Path, [Dir | Rest]) ->
    case lists:member(Dir, Path) of
        true  -> ok;
        false -> code:add_patha(Dir)
    end,
    add_paths_to_code_path1(Path, Rest);
add_paths_to_code_path1(_, []) ->
    ok.

%% -------------------------------------------------------------------
%% Code copied from `kernel/src/code_server.erl`.
%%
%% The goal is to mimic the behavior of the `$ERL_LIBS` environment
%% variable.

get_user_lib_dirs(Path) ->
    Sep = case os:type() of
              {win32, _} -> ";";
              _          -> ":"
          end,
    SplitPath = string:lexemes(Path, Sep),
    get_user_lib_dirs_1(SplitPath).

get_user_lib_dirs_1([Dir|DirList]) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Dirs} ->
            Paths = make_path(Dir, Dirs),
            %% Only add paths trailing with ./ebin.
            [P || P <- Paths, filename:basename(P) =:= "ebin"] ++
                get_user_lib_dirs_1(DirList);
        error ->
            get_user_lib_dirs_1(DirList)
    end;
get_user_lib_dirs_1([]) -> [].

%%
%% Create the initial path.
%%
make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, []).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
                     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split_base(BaseName) of
        {Name, VsnStr} ->
            case vsn_to_num(VsnStr) of
                {ok, VsnNum} ->
                    {Name,VsnNum,FullName};
                false ->
                    {FullName,[0],FullName}
            end;
        _ ->
            {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
        true ->
            {ok, [list_to_integer(S) || S <- string:lexemes(Vsn, ".")]};
        _  ->
            false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = string:lexemes(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true;
                  (_)                       -> false
              end, Cs).

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keyfind(Name, 1, Acc) of
        {_, NV, OldFullName} when NV =:= NumVsn ->
            case filename:extension(OldFullName) =:= ArchiveExt of
                false ->
                    choose(Bs,Acc, ArchiveExt);
                true ->
                    Acc2 = lists:keystore(Name, 1, Acc, New),
                    choose(Bs,Acc2, ArchiveExt)
            end;
        {_, _, _} ->
            choose(Bs,Acc, ArchiveExt);
        false ->
            choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

make_path(_, [], Res) ->
    Res;
make_path(BundleDir, [Bundle|Tail], Res) ->
    Dir = filename:append(BundleDir, Bundle),
    Ebin = filename:append(Dir, "ebin"),
    %% First try with /ebin
    case is_dir(Ebin) of
        true ->
            make_path(BundleDir, Tail, [Ebin|Res]);
        false ->
            %% Second try with archive
            Ext = archive_extension(),
            Base = filename:basename(Bundle, Ext),
            Ebin2 = filename:join([BundleDir, Base ++ Ext, Base, "ebin"]),
            Ebins =
                case split_base(Base) of
                    {AppName,_} ->
                        Ebin3 = filename:join([BundleDir, Base ++ Ext,
                                               AppName, "ebin"]),
                        [Ebin3, Ebin2, Dir];
                    _ ->
                        [Ebin2, Dir]
                end,
            case try_ebin_dirs(Ebins) of
                {ok,FoundEbin} ->
                    make_path(BundleDir, Tail, [FoundEbin|Res]);
                error ->
                    make_path(BundleDir, Tail, Res)
            end
    end.

try_ebin_dirs([Ebin|Ebins]) ->
    case is_dir(Ebin) of
        true -> {ok,Ebin};
        false -> try_ebin_dirs(Ebins)
    end;
try_ebin_dirs([]) ->
    error.

split_base(BaseName) ->
    case string:lexemes(BaseName, "-") of
        [_, _|_] = Toks ->
            Vsn = lists:last(Toks),
            AllButLast = lists:droplast(Toks),
            {string:join(AllButLast, "-"),Vsn};
        [_|_] ->
            BaseName
    end.

is_dir(Path) ->
    case erl_prim_loader:read_file_info(Path) of
        {ok,#file_info{type=directory}} -> true;
        _ -> false
    end.

archive_extension() ->
    init:archive_extension().

%% -------------------------------------------------------------------
%%
%% RABBITMQ_NODENAME
%%   Erlang node name.
%%   Default: rabbit@<hostname>
%%
%% RABBITMQ_USE_LONGNAME
%%   Flag indicating if long Erlang node names should be used instead
%%   of short ones.
%%   Default: unset (use short names)

node_name_and_type(Context) ->
    NameType = get_node_name_type(),
    Nodename = get_node_name(NameType),
    Context#{nodename => Nodename,
             split_nodename => rabbit_nodes_common:parts(Nodename),
             nodename_type => NameType}.

get_node_name_type() ->
    UseLongname = get_prefixed_env_var("RABBITMQ_USE_LONGNAME"),
    case UseLongname of
        false -> shortnames;
        Value -> case value_is_yes(Value) of
                     true  -> longnames;
                     false -> shortnames
                 end
    end.

get_node_name(NameType) ->
    LongHostname = net_adm:localhost(),
    ShortHostname = re:replace(LongHostname, "\\..*$", "", [{return, list}]),
    case get_prefixed_env_var("RABBITMQ_NODENAME") of
        false when NameType =:= shortnames ->
            rabbit_nodes_common:make({"rabbit", ShortHostname});
        false when NameType =:= longnames ->
            rabbit_nodes_common:make({"rabbit", LongHostname});
        Value ->
            case string:find(Value, "@") of
                nomatch when NameType =:= shortnames ->
                    rabbit_nodes_common:make({Value, ShortHostname});
                nomatch when NameType =:= longnames ->
                    rabbit_nodes_common:make({Value, LongHostname});
                _ ->
                    rabbit_nodes_common:make(Value)
            end
    end.

%% -------------------------------------------------------------------
%%
%% RABBITMQ_CONFIG_FILE
%%   Main configuration file.
%%   Extension is optional. `.config` for the old rlang-term-based
%%   format, `.conf` for the new Cuttlefish-based format.
%%   Default: (Unix) ${SYS_PREFIX}/etc/rabbitmq/rabbitmq
%%         (Windows) ${RABBITMQ_BASE}\rabbitmq
%%
%% RABBITMQ_ADVANCED_CONFIG_FILE
%%   Advanced configuration file.
%%   Erlang-term-based format with a `.config` extension.
%%   Default: (Unix) ${SYS_PREFIX}/etc/rabbitmq/advanced.config
%%         (Windows) ${RABBITMQ_BASE}\advanced.config

config_base_dir(Context) ->
    ConfigBaseDir = get_config_base_dir(Context),
    Context#{config_base_dir => ConfigBaseDir}.

get_config_base_dir(#{os_type := {unix, _}}) ->
    SysPrefix = get_sys_prefix(),
    Dir = filename:join([SysPrefix, "etc", "rabbitmq"]),
    normalize_path(Dir);
get_config_base_dir(#{os_type := {win32, _}}) ->
    Dir = get_rabbitmq_base(),
    normalize_path(Dir).

config_files(Context) ->
    MainConfigFile = get_main_config_file(Context),
    AdvancedConfigFile = get_advanced_config_file(Context),
    Context#{main_config_file => MainConfigFile,
             advanced_config_file => AdvancedConfigFile}.

get_main_config_file(Context) ->
    File = get_prefixed_env_var(
             "RABBITMQ_CONFIG_FILE",
             get_default_main_config_file(Context)),
    normalize_path(File).

get_default_main_config_file(#{config_base_dir := ConfigBaseDir}) ->
    filename:join(ConfigBaseDir, "rabbitmq").

get_advanced_config_file(Context) ->
    File = get_prefixed_env_var(
             "RABBITMQ_ADVANCED_CONFIG_FILE",
             get_default_advanced_config_file(Context)),
    normalize_path(File).

get_default_advanced_config_file(#{config_base_dir := ConfigBaseDir}) ->
    filename:join(ConfigBaseDir, "advanced.config").

%% -------------------------------------------------------------------
%%
%% RABBITMQ_LOG_BASE
%%   Directory to write log files
%%   Default: (Unix) ${SYS_PREFIX}/var/log/rabbitmq
%%         (Windows) ${RABBITMQ_BASE}\log
%%
%% RABBITMQ_LOGS
%%   Main log file
%%   Default: ${RABBITMQ_LOG_BASE}/${RABBITMQ_NODENAME}.log
%%
%% RABBITMQ_UPDATE_LOG
%%   Upgrade-procesure-specific log file
%%   Default: ${RABBITMQ_LOG_BASE}/${RABBITMQ_NODENAME}_upgrade.log
%%
%% RABBITMQ_LOG
%%   Log level; overrides the configuration file value
%%   Default: (undefined)
%%
%% RABBITMQ_DBG
%%   List of `module`, `module:function` or `module:function/arity`
%%   to watch with dbg.
%%   Default: (undefined)

log_levels(Context) ->
    LogLevels = get_log_levels(),
    Context#{log_levels => LogLevels}.

log_files(Context) ->
    LogBaseDir = get_log_base_dir(Context),
    MainLogFile = get_main_log_file(Context, LogBaseDir),
    UpgradeLogFile = get_upgrade_log_file(Context, LogBaseDir),
    Context#{log_base_dir => LogBaseDir,
             main_log_file => MainLogFile,
             upgrade_log_file => UpgradeLogFile}.

get_log_levels() ->
    LogValue = get_prefixed_env_var("RABBITMQ_LOG"),
    case LogValue of
        false -> undefined;
        _     -> get_log_levels1(string:lexemes(LogValue, ","), #{})
    end.

get_log_levels1([CategoryValue | Rest], Result) ->
    case string:lexemes(CategoryValue, "=") of
        ["+color"] ->
            Result1 = Result#{color => true},
            get_log_levels1(Rest, Result1);
        ["-color"] ->
            Result1 = Result#{color => false},
            get_log_levels1(Rest, Result1);
        [CategoryOrLevel] ->
            case parse_level(CategoryOrLevel) of
                undefined ->
                    Result1 = Result#{CategoryOrLevel => info},
                    get_log_levels1(Rest, Result1);
                Level ->
                    Result1 = Result#{global => Level},
                    get_log_levels1(Rest, Result1)
            end;
        [Category, Level0] ->
            case parse_level(Level0) of
                undefined ->
                    get_log_levels1(Rest, Result);
                Level ->
                    Result1 = Result#{Category => Level},
                    get_log_levels1(Rest, Result1)
            end
    end;
get_log_levels1([], Result) ->
    Result.

parse_level("debug")     -> debug;
parse_level("info")      -> info;
parse_level("notice")    -> notice;
parse_level("warning")   -> warning;
parse_level("error")     -> error;
parse_level("critical")  -> critical;
parse_level("alert")     -> alert;
parse_level("emergency") -> emergency;
parse_level("none")      -> none;
parse_level(_)           -> undefined.

get_log_base_dir(#{os_type := {unix, _}}) ->
    SysPrefix = get_sys_prefix(),
    Default = filename:join([SysPrefix, "var", "log", "rabbitmq"]),
    normalize_path(get_prefixed_env_var("RABBITMQ_LOG_BASE", Default));
get_log_base_dir(#{os_type := {win32, _}}) ->
    RabbitmqBase = get_rabbitmq_base(),
    Default = filename:join([RabbitmqBase, "log"]),
    normalize_path(get_prefixed_env_var("RABBITMQ_LOG_BASE", Default)).

get_main_log_file(#{nodename := Nodename}, LogBaseDir) ->
    Default = filename:join(LogBaseDir, atom_to_list(Nodename) ++ ".log"),
    Value = get_prefixed_env_var("RABBITMQ_LOGS", Default),
    case Value of
        "-" -> Value;
        _   -> normalize_path(Value)
    end.

get_upgrade_log_file(#{nodename := Nodename}, LogBaseDir) ->
    Default = filename:join(LogBaseDir,
                            atom_to_list(Nodename) ++ "_upgrade.log"),
    normalize_path(get_prefixed_env_var("RABBITMQ_UPGRADE_LOG", Default)).

dbg_config() ->
    {Mods, Output} = get_dbg_config(),
    #{dbg_output => Output,
      dbg_mods => Mods}.

dbg_config(Context) ->
    DbgContext = dbg_config(),
    maps:merge(Context, DbgContext).

get_dbg_config() ->
    Output = stdout,
    DbgValue = get_prefixed_env_var("RABBITMQ_DBG"),
    case DbgValue of
        false -> {[], Output};
        _     -> get_dbg_config1(string:lexemes(DbgValue, ","), [], Output)
    end.

get_dbg_config1(["=" ++ Filename | Rest], Mods, _) ->
    get_dbg_config1(Rest, Mods, Filename);
get_dbg_config1([SpecValue | Rest], Mods, Output) ->
    Pattern = "([^:]+)(?::([^/]+)(?:/([0-9]+))?)?",
    Options = [{capture, all_but_first, list}],
    Mods1 = case re:run(SpecValue, Pattern, Options) of
                {match, [M, F, A]} ->
                    Entry = {list_to_atom(M),
                             list_to_atom(F),
                             list_to_integer(A)},
                    [Entry | Mods];
                {match, [M, F]} ->
                    Entry = {list_to_atom(M),
                             list_to_atom(F),
                             '_'},
                    [Entry | Mods];
                {match, [M]} ->
                    Entry = {list_to_atom(M),
                             '_',
                             '_'},
                    [Entry | Mods];
                nomatch ->
                    Mods
            end,
    get_dbg_config1(Rest, Mods1, Output);
get_dbg_config1([], Mods, Output) ->
    {lists:reverse(Mods), Output}.

%% -------------------------------------------------------------------
%%
%% RABBITMQ_MNESIA_BASE
%%   Directory where to create Mnesia directory.
%%   Default: (Unix) ${SYS_PREFIX}/var/lib/rabbitmq/mnesia
%%         (Windows) ${RABBITMQ_BASE}/db
%%
%% RABBITMQ_MNESIA_DIR
%%   Directory where to put Mnesia data.
%%   Default: (Unix) ${RABBITMQ_MNESIA_BASE}/${RABBITMQ_NODENAME}
%%         (Windows) ${RABBITMQ_MNESIA_BASE}\${RABBITMQ_NODENAME}-mnesia

mnesia_dir(Context) ->
    MnesiaBaseDir = get_mnesia_base_dir(Context),
    MnesiaDir = get_mnesia_dir(Context, MnesiaBaseDir),
    Context#{mnesia_base_dir => MnesiaBaseDir,
             mnesia_dir => MnesiaDir}.

get_mnesia_base_dir(#{from_remote_node := Remote}) ->
    case get_prefixed_env_var("RABBITMQ_MNESIA_BASE") of
        false when Remote =:= offline ->
            undefined;
        false ->
            get_mnesia_base_dir_from_node(Remote);
        Dir ->
            normalize_path(Dir)
    end;
get_mnesia_base_dir(Context) ->
    get_mnesia_base_dir_from_env(Context).

get_mnesia_base_dir_from_env(Context) ->
    Default = get_default_mnesia_base_dir(Context),
    Dir = get_prefixed_env_var("RABBITMQ_MNESIA_BASE", Default),
    normalize_path(Dir).

get_mnesia_base_dir_from_node(_Remote) ->
    %% This variable is used to compute other variables but is not
    %% stored anywhere. Therefore, we can't know what a remote node used
    %% initially.
    %%
    %% That's ok because we don't really need to know that value anyway:
    %% only the variables based on it are relevant.
    undefined.

get_default_mnesia_base_dir(#{data_dir := DataDir} = Context) ->
    Basename = case Context of
                   #{os_type := {unix, _}}  -> "mnesia";
                   #{os_type := {win32, _}} -> "db"
               end,
    filename:join(DataDir, Basename).

get_mnesia_dir(#{from_remote_node := Remote}, _) ->
    case get_prefixed_env_var("RABBITMQ_MNESIA_DIR") of
        false when Remote =:= offline ->
            undefined;
        false ->
            get_mnesia_dir_from_node(Remote);
        Dir ->
            normalize_path(Dir)
    end;
get_mnesia_dir(Context, MnesiaBaseDir) ->
    get_mnesia_dir_from_env(Context, MnesiaBaseDir).

get_mnesia_dir_from_env(Context, MnesiaBaseDir) ->
    Dir = get_prefixed_env_var(
            "RABBITMQ_MNESIA_DIR",
            get_default_mnesia_dir(Context, MnesiaBaseDir)),
    normalize_path(Dir).

get_mnesia_dir_from_node(Remote) ->
    Ret = query_remote(Remote, application, get_env, [mnesia, dir]),
    case Ret of
        {ok, undefined} ->
            throw({query, Remote, {mnesia, dir, undefined}});
        {ok, {ok, Dir}} ->
            normalize_path(Dir);
        {badrpc, nodedown} ->
            undefined
    end.

get_default_mnesia_dir(#{os_type := {unix, _}, nodename := Nodename}, MnesiaBaseDir) ->
    filename:join(MnesiaBaseDir, atom_to_list(Nodename));
get_default_mnesia_dir(#{os_type := {win32, _}, nodename := Nodename}, MnesiaBaseDir) ->
    filename:join(MnesiaBaseDir, atom_to_list(Nodename) ++ "-mnesia").

%% -------------------------------------------------------------------
%%
%% RABBITMQ_QUORUM_DIR
%%   Directory where to store Ra state for quorum queues.
%%   Default: ${RABBITMQ_MNESIA_DIR}/quorum

quorum_dir(Context) ->
    QuorumQueueDir = get_quorum_queue_dir(Context),
    Context#{quorum_queue_dir => QuorumQueueDir}.

get_quorum_queue_dir(#{mnesia_dir := MnesiaDir}) ->
    Default = filename:join(MnesiaDir, "quorum"),
    Dir = get_prefixed_env_var("RABBITMQ_QUORUM_DIR", Default),
    normalize_path(Dir).

%% -------------------------------------------------------------------
%%
%% RABBITMQ_PID_FILE
%%   File used to write the Erlang VM OS PID.
%%   Default: ${RABBITMQ_MNESIA_DIR}.pid
%%
%% RABBITMQ_KEEP_PID_FILE_ON_EXIT
%%   Whether to keep or remove the PID file on Erlang VM exit.
%%   Default: true

pid_file(Context) ->
    PidFile = get_pid_file_path(Context),
    KeepPidFile = keep_pid_file_on_exit(),
    Context#{pid_file => PidFile,
             keep_pid_file_on_exit => KeepPidFile}.

get_pid_file_path(#{mnesia_base_dir := MnesiaBaseDir,
                    nodename := Nodename}) ->
    File = get_prefixed_env_var(
             "RABBITMQ_PID_FILE",
             filename:join(MnesiaBaseDir, atom_to_list(Nodename) ++ ".pid")),
    normalize_path(File).

keep_pid_file_on_exit() ->
    case get_prefixed_env_var("RABBITMQ_KEEP_PID_FILE_ON_EXIT") of
        false -> false;
        Value -> value_is_yes(Value)
    end.

%% -------------------------------------------------------------------
%%
%% RABBITMQ_FEATURE_FLAGS_FILE
%%   File used to store enabled feature flags.
%%   Default: ${RABBITMQ_MNESIA_BASE}/${RABBITMQ_NODENAME}-feature_flags

feature_flags_file(Context) ->
    FFFile = get_feature_flags_file(Context),
    Context#{feature_flags_file => FFFile}.

get_feature_flags_file(#{from_remote_node := Remote}) ->
    case get_prefixed_env_var("RABBITMQ_FEATURE_FLAGS_FILE") of
        false when Remote =:= offline ->
            undefined;
        false ->
            get_feature_flags_file_from_node(Remote);
        File ->
            normalize_path(File)
    end;
get_feature_flags_file(Context) ->
    get_feature_flags_file_from_env(Context).

get_feature_flags_file_from_env(#{mnesia_base_dir := MnesiaBaseDir,
                                  nodename := Nodename}) ->
    Default = filename:join(MnesiaBaseDir,
                            atom_to_list(Nodename) ++ "-feature_flags"),
    File = get_env_var("RABBITMQ_FEATURE_FLAGS_FILE", Default),
    normalize_path(File).

get_feature_flags_file_from_node(Remote) ->
    Ret = query_remote(Remote,
                        application, get_env, [rabbit, feature_flags_file]),
    case Ret of
        {ok, undefined} ->
            throw({query, Remote, {rabbit, feature_flags_file, undefined}});
        {ok, {ok, File}} ->
            normalize_path(File);
        {badrpc, nodedown} ->
            undefined
    end.

%% -------------------------------------------------------------------
%%
%% RABBITMQ_PLUGINS_DIR
%%   List of directories where to look for plugins.
%%   Directories are separated by:
%%     ':' on Unix
%%     ';' on Windows
%%   Default: ${RABBITMQ_HOME}/plugins
%%
%% RABBITMQ_PLUGINS_EXPAND_DIR
%%   Directory where to expand plugin archives.
%%   Default: ${RABBITMQ_MNESIA_BASE}/${RABBITMQ_NODENAME}-plugins-expand
%%
%% RABBITMQ_ENABLED_PLUGINS_FILE
%%   File where the list of enabled plugins is stored.
%%   Default: (Unix) ${SYS_PREFIX}/etc/rabbitmq/enabled_plugins
%%         (Windows) ${RABBITMQ_BASE}\enabled_plugins
%%
%% RABBITMQ_ENABLED_PLUGINS
%%   List of plugins to enable on startup.
%%   Values are:
%%     "ALL" to enable all plugins
%%     "NONE" to enable no plugin
%%     a list of plugin names, separated by a coma (',')
%%   Default: Empty (i.e. use ${RABBITMQ_ENABLED_PLUGINS_FILE})

plugins_dirs(Context) ->
    PluginsPath = get_plugins_path(Context),
    PluginsExpandDir = get_plugins_expand_dir(Context),
    EnabledPluginsFile = get_enabled_plugins_file(Context),
    EnabledPlugins = get_enabled_plugins(),
    Context#{plugins_path => PluginsPath,
             plugins_expand_dir => PluginsExpandDir,
             enabled_plugins_file => EnabledPluginsFile,
             enabled_plugins => EnabledPlugins}.

get_plugins_path(#{from_remote_node := Remote}) ->
    case get_prefixed_env_var("RABBITMQ_PLUGINS_DIR") of
        false when Remote =:= offline ->
            undefined;
        false ->
            get_plugins_path_from_node(Remote);
        Path ->
            Path
    end;
get_plugins_path(Context) ->
    get_plugins_path_from_env(Context).

get_plugins_path_from_env(Context) ->
    get_prefixed_env_var(
      "RABBITMQ_PLUGINS_DIR", get_default_plugins_path_from_env(Context)).

get_plugins_path_from_node(Remote) ->
    Ret = query_remote(Remote, application, get_env, [rabbit, plugins_dir]),
    case Ret of
        {ok, undefined} ->
            throw({query, Remote, {rabbit, plugins_dir, undefined}});
        {ok, {ok, Path}} ->
            Path;
        {badrpc, nodedown} ->
            undefined
    end.

get_default_plugins_path(#{from_remote_node := offline}) ->
    undefined;
get_default_plugins_path(#{from_remote_node := Remote}) ->
    get_default_plugins_path_from_node(Remote);
get_default_plugins_path(Context) ->
    get_default_plugins_path_from_env(Context).

get_default_plugins_path_from_env(#{os_type := OSType}) ->
    ThisModDir = this_module_dir(),
    PluginsDir = case filename:basename(ThisModDir) of
                     "ebin" ->
                         filename:dirname(
                           filename:dirname(
                             filename:dirname(ThisModDir)));
                     _ ->
                         filename:join(
                           filename:dirname(
                             filename:dirname(ThisModDir)),
                           "plugins")
                 end,
    case {OSType, PluginsDir} of
        {{unix, _}, "/usr/lib/rabbitmq/" ++ _} ->
            UserPluginsDir = filename:join(
                               ["/", "usr", "lib", "rabbitmq", "plugins"]),
            UserPluginsDir ++ ":" ++ PluginsDir;
        _ ->
            PluginsDir
    end.

get_default_plugins_path_from_node(Remote) ->
    Ret = query_remote(Remote, code, lib_dir, [rabbit_common]),
    case Ret of
        {ok, {error, _} = Error} ->
            throw({query, Remote, {code, lib_dir, Error}});
        {ok, Path} ->
            filename:dirname(Path);
        {badrpc, nodedown} ->
            undefined
    end.

get_plugins_expand_dir(#{mnesia_base_dir := MnesiaBaseDir,
                         nodename := Nodename}) ->
    Dir = get_prefixed_env_var(
            "RABBITMQ_PLUGINS_EXPAND_DIR",
            filename:join(MnesiaBaseDir,
                          atom_to_list(Nodename) ++ "-plugins-expand")),
    normalize_path(Dir).

get_enabled_plugins_file(#{from_remote_node := Remote}) ->
    case get_prefixed_env_var("RABBITMQ_ENABLED_PLUGINS_FILE") of
        false when Remote =:= offline ->
            undefined;
        false ->
            get_enabled_plugins_file_from_node(Remote);
        File ->
            normalize_path(File)
    end;
get_enabled_plugins_file(Context) ->
    get_enabled_plugins_file_from_env(Context).

get_enabled_plugins_file_from_env(Context) ->
    File = get_prefixed_env_var(
             "RABBITMQ_ENABLED_PLUGINS_FILE",
             get_default_enabled_plugins_file(Context)),
    normalize_path(File).

get_default_enabled_plugins_file(#{config_base_dir := ConfigBaseDir}) ->
    filename:join(ConfigBaseDir, "enabled_plugins").

get_enabled_plugins_file_from_node(Remote) ->
    Ret = query_remote(Remote,
                       application, get_env, [rabbit, enabled_plugins_file]),
    case Ret of
        {ok, undefined} ->
            throw({query, Remote, {rabbit, enabled_plugins_file, undefined}});
        {ok, {ok, File}} ->
            File;
        {badrpc, nodedown} ->
            undefined
    end.

get_enabled_plugins() ->
    case get_prefixed_env_var("RABBITMQ_ENABLED_PLUGINS") of
        false  -> undefined;
        "ALL"  -> all;
        "NONE" -> [];
        Value  -> [list_to_atom(P) || P <- string:lexemes(Value, ",")]
    end.

%% -------------------------------------------------------------------
%%
%% RABBITMQ_NODE_IP_ADDRESS
%%   AMQP TCP IP address to listen on
%%   Default: unset (i.e. listen on all interfaces)
%%
%% RABBITMQ_NODE_PORT
%%   AMQP TCP port.
%%   Default: 5672
%%
%% RABBITMQ_DIST_PORT
%%   Erlang distribution TCP port.
%%   Default: ${RABBITMQ_NODE_PORT} + 20000

tcp_configuration(Context) ->
    AmqpIpAddress = get_amqp_ipaddr(),
    AmqpTcpPort = get_amqp_tcp_port(),
    DistTcpPort = get_erlang_dist_tcp_port(AmqpTcpPort),
    Context#{amqp_ipaddr_and_tcp_port => {AmqpIpAddress, AmqpTcpPort},
             erlang_dist_tcp_port => DistTcpPort}.

get_amqp_ipaddr() ->
    get_prefixed_env_var("RABBITMQ_NODE_IP_ADDRESS", "auto").

get_amqp_tcp_port() ->
    Default = 5672,
    case get_prefixed_env_var("RABBITMQ_NODE_PORT") of
        false ->
            Default;
        TcpPortStr ->
            try
                erlang:list_to_integer(TcpPortStr)
            catch
                _:badarg ->
                    rabbit_log_prelaunch:error(
                      "Invalid value for $RABBITMQ_NODE_PORT: ~p",
                      [TcpPortStr]),
                    throw({exit, ex_config})
            end
    end.

get_erlang_dist_tcp_port(AmqpTcpPort) ->
    Default = AmqpTcpPort + 20000,
    case get_prefixed_env_var("RABBITMQ_DIST_PORT") of
        false ->
            Default;
        TcpPortStr ->
            try
                erlang:list_to_integer(TcpPortStr)
            catch
                _:badarg ->
                    rabbit_log_prelaunch:error(
                      "Invalid value for $RABBITMQ_DIST_PORT: ~p",
                      [TcpPortStr]),
                    throw({exit, ex_config})
            end
    end.

%% -------------------------------------------------------------------
%%
%%  SYS_PREFIX [Unix only]
%%    Default: ""
%%
%%  RABBITMQ_BASE [Windows only]
%%    Directory where to put RabbitMQ data.
%%    Default: !APPDATA!\RabbitMQ

data_dir(Context) ->
    DataDir = get_rabbitmq_data_dir(Context),
    RabbitmqHome = get_rabbitmq_home(Context),
    Context#{data_dir => DataDir,
             rabbitmq_home => RabbitmqHome}.

get_rabbitmq_data_dir(#{os_type := {unix, _}}) ->
    SysPrefix = get_sys_prefix(),
    filename:join([SysPrefix, "var", "lib", "rabbitmq"]);
get_rabbitmq_data_dir(#{os_type := {win32, _}}) ->
    get_rabbitmq_base().

get_sys_prefix() ->
    Dir = get_env_var("SYS_PREFIX", ""),
    normalize_path(Dir).

get_rabbitmq_base() ->
    Dir = case get_env_var("RABBITMQ_BASE") of
              false ->
                  AppData = get_env_var("APPDATA"),
                  filename:join(AppData, "RabbitMQ");
              Value ->
                  Value
          end,
    normalize_path(Dir).

get_rabbitmq_home(Context) ->
    Dir = case get_env_var("RABBITMQ_HOME") of
              false -> filename:dirname(get_default_plugins_path(Context));
              Value -> Value
          end,
    normalize_path(Dir).

%% -------------------------------------------------------------------
%%
%%  RABBITMQ_ALLOW_INPUT
%%    Indicate if an Erlang shell is started or not.
%%    Default: false

shell(Context) ->
    IsInteractive = get_allow_input(),
    ColorSupport = does_output_support_color(Context),
    Context#{interactive_shell => IsInteractive,
             output_supports_colors => ColorSupport}.

get_allow_input() ->
    value_is_yes(get_env_var("RABBITMQ_ALLOW_INPUT")).

% FIXME: We would need a way to call isatty(3) to make sure the output
% is a terminal.
does_output_support_color(#{os_type := {unix, _}}) -> true;
does_output_support_color(#{os_type := {win32, _}}) -> false.

%% -------------------------------------------------------------------
%% Loading of rabbitmq-env.conf.
%% -------------------------------------------------------------------

load_conf_env_file(#{os_type := {unix, _}} = Context) ->
    SysPrefix = get_sys_prefix(),
    Default = filename:join(
                [SysPrefix, "etc", "rabbitmq", "rabbitmq-env.conf"]),
    ConfEnvFile = get_prefixed_env_var("RABBITMQ_CONF_ENV_FILE", Default),
    Context1 = Context#{conf_env_file => ConfEnvFile},
    case loading_conf_env_file_enabled(Context1) of
        true ->
            case filelib:is_regular(ConfEnvFile) of
                false ->
                    rabbit_log_prelaunch:debug(
                      "No $RABBITMQ_CONF_ENV_FILE (~ts)", [ConfEnvFile]),
                    Context1;
                true ->
                    case os:find_executable("sh") of
                        false -> Context1;
                        Sh    -> do_load_conf_env_file(Context1,
                                                       Sh,
                                                       ConfEnvFile)
                    end
            end;
        false ->
            rabbit_log_prelaunch:debug(
              "Loading of $RABBITMQ_CONF_ENV_FILE (~ts) is disabled",
              [ConfEnvFile]),
            Context1
    end;
load_conf_env_file(#{os_type := {win32, _}} = Context) ->
    RabbitmqBase = get_rabbitmq_base(),
    Default = filename:join(
                [RabbitmqBase, "rabbitmq-env-conf.bat"]),
    ConfEnvFile = get_prefixed_env_var("RABBITMQ_CONF_ENV_FILE", Default),
    Context1 = Context#{conf_env_file => ConfEnvFile},
    case loading_conf_env_file_enabled(Context1) of
        true ->
            rabbit_log_prelaunch:notice(
              "Loading of $RABBITMQ_CONF_ENV_FILE (~s) "
              "is not implemented for Windows",
              [ConfEnvFile]),
            Context1;
        false ->
            rabbit_log_prelaunch:debug(
              "Loading of $RABBITMQ_CONF_ENV_FILE (~ts) is disabled",
              [ConfEnvFile]),
            Context1
    end;
load_conf_env_file(Context) ->
    Context.

-spec loading_conf_env_file_enabled(map()) -> boolean().

-ifdef(TEST).
loading_conf_env_file_enabled(_) ->
    persistent_term:get({?MODULE, load_conf_env_file}, true).
-else.
loading_conf_env_file_enabled(_) ->
    %% When this module is built without `TEST` defined, we want this
    %% function to always return true. However, this makes Dialyzer
    %% think it can only return true: this is not the case when the
    %% module is compiled with `TEST` defined. The following line is
    %% here to trick Dialyzer.
    erlang:get({?MODULE, always_undefined}) =:= undefined.
-endif.

do_load_conf_env_file(Context, Sh, ConfEnvFile) ->
    rabbit_log_prelaunch:debug(
      "Sourcing $RABBITMQ_CONF_ENV_FILE: ~ts", [ConfEnvFile]),
    Marker = rabbit_misc:format(
               "-----BEGIN VARS LIST FOR PID ~s-----", [os:getpid()]),
    Script = rabbit_misc:format(
               ". \"~ts\" && "
               "echo \"~s\" && "
               "set", [ConfEnvFile, Marker]),
    Args = ["-ex", "-c", Script],

    SysPrefix = get_sys_prefix(),
    RabbitmqHome = get_rabbitmq_home(Context),
    MainConfigFile = re:replace(
                       get_default_main_config_file(Context),
                       "\\.(conf|config)$", "", [{return, list}]),
    Env = [
           {"SYS_PREFIX", SysPrefix},
           {"RABBITMQ_HOME", RabbitmqHome},
           {"CONFIG_FILE", MainConfigFile},
           {"ADVANCED_CONFIG_FILE", get_default_advanced_config_file(Context)},
           {"MNESIA_BASE", get_default_mnesia_base_dir(Context)},
           {"ENABLED_PLUGINS_FILE", get_default_enabled_plugins_file(Context)},
           {"PLUGINS_DIR", get_default_plugins_path_from_env(Context)},
           {"CONF_ENV_FILE_PHASE", "rabbtimq-prelaunch"}
          ],

    Port = erlang:open_port(
             {spawn_executable, Sh},
             [{args, Args},
              {env, Env},
              binary,
              use_stdio,
              stderr_to_stdout,
              exit_status]),
    collect_sh_output(Context, Port, Marker, <<>>).

collect_sh_output(Context, Port, Marker, Output) ->
    receive
        {Port, {exit_status, ExitStatus}} ->
            rabbit_log_prelaunch:debug(
              "$RABBITMQ_CONF_ENV_FILE exit status: ~b", [ExitStatus]),
            DecodedOutput = unicode:characters_to_list(Output),
            Lines = string:split(string:trim(DecodedOutput), "\n", all),
            rabbit_log_prelaunch:debug("$RABBITMQ_CONF_ENV_FILE output:"),
            [rabbit_log_prelaunch:debug("  ~ts", [Line])
             || Line <- Lines],
            case ExitStatus of
                0 -> parse_conf_env_file_output(Context, Marker, Lines);
                _ -> Context
            end;
        {Port, {data, Chunk}} ->
            collect_sh_output(Context, Port, Marker, [Output, Chunk])
    end.

parse_conf_env_file_output(Context, _, []) ->
    Context;
parse_conf_env_file_output(Context, Marker, [Marker | Lines]) ->
    %% Found our marker, let's parse variables.
    parse_conf_env_file_output1(Context, Lines, #{});
parse_conf_env_file_output(Context, Marker, [_ | Lines]) ->
    parse_conf_env_file_output(Context, Marker, Lines).

parse_conf_env_file_output1(Context, [], Vars) ->
    %% Re-export variables.
    lists:foreach(
      fun(Var) ->
              IsUsed = var_is_used(Var),
              IsSet = var_is_set(Var),
              case IsUsed andalso not IsSet of
                  true ->
                      rabbit_log_prelaunch:debug(
                        "$RABBITMQ_CONF_ENV_FILE: re-exporting variable $~s",
                        [Var]),
                      os:putenv(Var, maps:get(Var, Vars));
                  false ->
                      ok
              end
      end, lists:sort(maps:keys(Vars))),
    Context;
parse_conf_env_file_output1(Context, [Line | Lines], Vars) ->
    SetXOutput = is_sh_set_x_output(Line),
    ShFunction = is_sh_function(Line, Lines),
    if
        SetXOutput ->
            parse_conf_env_file_output1(Context, Lines, Vars);
        ShFunction ->
            skip_sh_function(Context, Lines, Vars);
        true ->
            case string:split(Line, "=") of
                [Var, IncompleteValue] ->
                    {Value, Lines1} = parse_sh_literal(IncompleteValue, Lines),
                    Vars1 = Vars#{Var => Value},
                    parse_conf_env_file_output1(Context, Lines1, Vars1);
                _ ->
                    %% Parsing failed somehow.
                    rabbit_log_prelaunch:warning(
                      "Failed to parse $RABBITMQ_CONF_ENV_FILE output: ~p",
                      [Line]),
                    Context
            end
    end.

is_sh_set_x_output(Line) ->
    re:run(Line, "^\\++ ", [{capture, none}]) =:= match.

is_sh_function(_, []) ->
    false;
is_sh_function(Line, Lines) ->
    re:run(Line, "\\s\\(\\)\\s*$", [{capture, none}]) =:= match
    andalso
    re:run(hd(Lines), "^\\s*\\{\\s*$", [{capture, none}]) =:= match.

parse_sh_literal("'" ++ SingleQuoted, Lines) ->
    parse_single_quoted_literal(SingleQuoted, Lines, "");
parse_sh_literal("$'" ++ DollarSingleQuoted, Lines) ->
    parse_dollar_single_quoted_literal(DollarSingleQuoted, Lines, "");
parse_sh_literal(Unquoted, Lines) ->
    {Unquoted, Lines}.

parse_single_quoted_literal([$'], Lines, Literal) ->
    %% We reached the closing single quote.
    {lists:reverse(Literal), Lines};
parse_single_quoted_literal([], [Line | Lines], Literal) ->
    %% We reached the end of line before finding the closing single
    %% quote. The literal continues on the next line and includes that
    %% newline character.
    parse_single_quoted_literal(Line, Lines, [$\n | Literal]);
parse_single_quoted_literal([C | Rest], Lines, Literal) ->
    parse_single_quoted_literal(Rest, Lines, [C | Literal]).

parse_dollar_single_quoted_literal([$'], Lines, Literal) ->
    %% We reached the closing single quote.
    {lists:reverse(Literal), Lines};
parse_dollar_single_quoted_literal([], [Line | Lines], Literal) ->
    %% We reached the end of line before finding the closing single
    %% quote. The literal continues on the next line and includes that
    %% newline character.
    parse_dollar_single_quoted_literal(Line, Lines, [$\n | Literal]);
parse_dollar_single_quoted_literal([C | Rest], Lines, Literal) ->
    parse_dollar_single_quoted_literal(Rest, Lines, [C | Literal]).

skip_sh_function(Context, ["}" | Lines], Vars) ->
    parse_conf_env_file_output1(Context, Lines, Vars);
skip_sh_function(Context, [_ | Lines], Vars) ->
    skip_sh_function(Context, Lines, Vars).

%% -------------------------------------------------------------------
%% Helpers.
%% -------------------------------------------------------------------

get_env_var(VarName) ->
    case os:getenv(VarName) of
        false -> false;
        ""    -> false;
        Value -> Value
    end.

get_env_var(VarName, DefaultValue) ->
    case get_env_var(VarName) of
        false -> DefaultValue;
        Value -> Value
    end.

get_prefixed_env_var("RABBITMQ_" ++ Suffix = VarName) ->
    case get_env_var(VarName) of
        false -> get_env_var(Suffix);
        Value -> Value
    end.

get_prefixed_env_var(VarName, DefaultValue) ->
    case get_prefixed_env_var(VarName) of
        false -> DefaultValue;
        Value -> Value
    end.

var_is_used("RABBITMQ_" ++ _ = PrefixedVar) ->
    lists:member(PrefixedVar, ?USED_ENV_VARS);
var_is_used(Var) ->
    lists:member("RABBITMQ_" ++ Var, ?USED_ENV_VARS).

var_is_set("RABBITMQ_" ++ Var = PrefixedVar) ->
    os:getenv(PrefixedVar) /= false orelse
    os:getenv(Var) /= false;
var_is_set(Var) ->
    os:getenv("RABBITMQ_" ++ Var) /= false orelse
    os:getenv(Var) /= false.

value_is_yes(Value) when is_list(Value) orelse is_binary(Value) ->
    Options = [{capture, none}, caseless],
    re:run(string:trim(Value), "^(1|yes|true)$", Options) =:= match;
value_is_yes(Value) when is_boolean(Value) ->
    Value;
value_is_yes(_) ->
    false.

normalize_path("" = Path) ->
    Path;
normalize_path(Path) ->
    filename:join(filename:split(Path)).

this_module_dir() ->
    {_, _, File} = code:get_object_code(?MODULE),
    %% Possible locations:
    %%   - the rabbit_common plugin:
    %%     .../plugins/rabbit_common-$version.ez/rabbit_common-$version/ebin
    %%   - the CLI:
    %%     .../escript/$cli
    filename:dirname(File).

maybe_setup_dist_for_remote_query(
  #{from_remote_node := offline} = Context) ->
    Context;
maybe_setup_dist_for_remote_query(
  #{from_remote_node := {RemoteNode, _}} = Context) ->
    {NamePart, HostPart} = rabbit_nodes_common:parts(RemoteNode),
    NameType = case string:find(HostPart, ".") of
                   nomatch -> shortnames;
                   _       -> longnames
               end,
    ok = rabbit_nodes_common:ensure_epmd(),
    Context1 = setup_dist_for_remote_query(
                 Context, NamePart, HostPart, NameType, 50),
    case is_rabbitmq_loaded_on_remote_node(Context1) of
        true  -> Context1;
        false -> maybe_stop_dist_for_remote_query(
                   Context#{from_remote_node => offline})
    end;
maybe_setup_dist_for_remote_query(Context) ->
    Context.

setup_dist_for_remote_query(
  #{dist_started_for_remote_query := true} = Context,
  _, _, _, _) ->
    Context;
setup_dist_for_remote_query(Context, _, _, _, 0) ->
    Context;
setup_dist_for_remote_query(#{from_remote_node := {Remote, _}} = Context,
                            NamePart, HostPart, NameType,
                            Attempts) ->
    RndNamePart = NamePart ++ "_ctl_" ++ integer_to_list(rand:uniform(100)),
    Nodename = rabbit_nodes_common:make({RndNamePart, HostPart}),
    case net_kernel:start([Nodename, NameType]) of
        {ok, _} ->
            Context#{dist_started_for_remote_query => true};
        {error, {already_started, _}} ->
            Context;
        {error, {{already_started, _}, _}} ->
            Context;
        Error ->
            logger:error(
              "rabbit_env: Failed to setup distribution (as ~s) to "
              "query node ~s: ~p",
              [Nodename, Remote, Error]),
            setup_dist_for_remote_query(Context,
                                        NamePart, HostPart, NameType,
                                        Attempts - 1)
    end.

is_rabbitmq_loaded_on_remote_node(
  #{from_remote_node := Remote}) ->
    case query_remote(Remote, application, loaded_applications, []) of
        {ok, Apps} ->
            lists:keymember(mnesia, 1, Apps) andalso
            lists:keymember(rabbit, 1, Apps);
        _ ->
            false
    end.

maybe_stop_dist_for_remote_query(
  #{dist_started_for_remote_query := true} = Context) ->
    net_kernel:stop(),
    maps:remove(dist_started_for_remote_query, Context);
maybe_stop_dist_for_remote_query(Context) ->
    Context.

query_remote({RemoteNode, Timeout}, Mod, Func, Args) ->
    Ret = rpc:call(RemoteNode, Mod, Func, Args, Timeout),
    case Ret of
        {badrpc, nodedown} = Error -> Error;
        {badrpc, _} = Error        -> throw({query, RemoteNode, Error});
        _                          -> {ok, Ret}
    end.
