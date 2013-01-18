%%%-------------------------------------------------------------------
%%% @author KernelPanic <alevandal@kernelpanic.svyazcom.ru>
%%% @copyright (C) 2012, KernelPanic
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by KernelPanic <alevandal@kernelpanic.svyazcom.ru>
%%%-------------------------------------------------------------------
-module(ghost_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {ghost, {ghost, start_link, []},
	      Restart, Shutdown, Type, [ghost]},

    {ok, {SupFlags, [AChild]}}.

start_child(From, CallbackSpec) ->
    supervisor:start_child(?SERVER, [From, CallbackSpec]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
