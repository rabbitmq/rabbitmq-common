%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2020 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_ssl_options).

-export([fix/1]).


-define(BAD_SSL_PROTOCOL_VERSIONS, [
                                    %% POODLE
                                    sslv3,

                                    %% Client side of TLS 1.3 is not yet
                                    %% implemented in Erlang/OTP 22.0
                                    %% prereleases. As a consequence,
                                    %% not sure about the stability of
                                    %% the server side.
                                    %%
                                    %% FIXME: Revisit this decision when
                                    %% Erlang/OTP 22.0 final release is
                                    %% out.
                                    'tlsv1.3'
                                   ]).

-spec fix(rabbit_types:infos()) -> rabbit_types:infos().

fix(Config) ->
    fix_verify_fun(fix_ssl_protocol_versions(Config)).

fix_verify_fun(SslOptsConfig) ->
    %% Starting with ssl 4.0.1 in Erlang R14B, the verify_fun function
    %% takes 3 arguments and returns a tuple.
    case rabbit_misc:pget(verify_fun, SslOptsConfig) of
        {Module, Function, InitialUserState} ->
            Fun = make_verify_fun(Module, Function, InitialUserState),
            rabbit_misc:pset(verify_fun, Fun, SslOptsConfig);
        {Module, Function} when is_atom(Module) ->
            Fun = make_verify_fun(Module, Function, none),
            rabbit_misc:pset(verify_fun, Fun, SslOptsConfig);
        {Verifyfun, _InitialUserState} when is_function(Verifyfun, 3) ->
            SslOptsConfig;
        undefined ->
            SslOptsConfig
    end.

make_verify_fun(Module, Function, InitialUserState) ->
    try
        %% Preload the module: it is required to use
        %% erlang:function_exported/3.
        Module:module_info()
    catch
        _:Exception ->
            rabbit_log:error("SSL verify_fun: module ~s missing: ~p~n",
                             [Module, Exception]),
            throw({error, {invalid_verify_fun, missing_module}})
    end,
    NewForm = erlang:function_exported(Module, Function, 3),
    OldForm = erlang:function_exported(Module, Function, 1),
    case {NewForm, OldForm} of
        {true, _} ->
            %% This verify_fun is supported by Erlang R14B+ (ssl
            %% 4.0.1 and later).
            Fun = fun(OtpCert, Event, UserState) ->
                    Module:Function(OtpCert, Event, UserState)
            end,
            {Fun, InitialUserState};
        {_, true} ->
            %% This verify_fun is supported by Erlang R14B+ for
            %% undocumented backward compatibility.
            %%
            %% InitialUserState is ignored in this case.
            fun(Args) ->
                    Module:Function(Args)
            end;
        _ ->
            rabbit_log:error("SSL verify_fun: no ~s:~s/3 exported~n",
              [Module, Function]),
            throw({error, {invalid_verify_fun, function_not_exported}})
    end.

fix_ssl_protocol_versions(Config) ->
    case application:get_env(rabbit, ssl_allow_poodle_attack) of
        {ok, true} ->
            Config;
        _ ->
            Configured = case rabbit_misc:pget(versions, Config) of
                             undefined -> rabbit_misc:pget(available,
                                                           ssl:versions(),
                                                           []);
                             Vs        -> Vs
                         end,
            rabbit_misc:pset(versions, Configured -- ?BAD_SSL_PROTOCOL_VERSIONS, Config)
    end.
