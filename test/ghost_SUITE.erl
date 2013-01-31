
-module(ghost_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% ghost
-export([foo_noreply/0, foo_reply/0, foo_reply_cast/0, foo_reply_call/0]).

-export([app_start_stop/1]).
-export([app_not_started/1]).
-export([app_already_started/1]).

-export([worker_start/1]).
-export([worker_correct_finish/1]).
-export([worker_doit2_reply/1]).
-export([worker_doit3_reply/1]).

-define(WAIT_TIMEOUT, 3000).

all() ->
    [{group, app}, {group, worker}].

groups() ->
    [{app, [
	    app_start_stop,
	    app_not_started,
	    app_already_started
	   ]},
     {worker, [
	       worker_start,
	       worker_correct_finish,
	       worker_doit2_reply,
	       worker_doit3_reply
	      ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_group(app, Config) ->
    Config;
init_per_group(worker, Config) ->
    ok = ghost:start(),
    [{wait_timeout, ?WAIT_TIMEOUT} | Config].

end_per_group(app, _) ->
    ok;
end_per_group(worker, _) ->
    ok = ghost:stop(),
    ok.

-define(REPLY_OK, {reply, ok}).

foo_noreply() ->
    noreply.

foo_reply() ->
    {reply, ?REPLY_OK}.

foo_reply_cast() ->
    {reply_cast, ?REPLY_OK}.

foo_reply_call() ->
    {reply_call, ?REPLY_OK}.

foo_noreply_wait() ->
    receive
	shoot ->
	    noreply
    end.

app_start_stop(_Config) ->
    ok = ghost:start(),
    ok = ghost:stop(),
    ok.

app_not_started(_Config) ->
    {error, ghost_not_started} = ghost:doit({ghost_SUITE, foo_noreply, []}),
    ok.

app_already_started(_Config) ->
    ok = ghost:start(),
    {error, {already_started, ghost}} = ghost:start(),
    ok = ghost:stop(),
    ok.

worker_start(_Config) ->
    {ok, _Pid} = ghost:doit(ghost_SUITE, foo_noreply, []),
    ok.

worker_correct_finish(Config) ->
    Timeout = ?config(wait_timeout, Config),
    {ok, Pid} = ghost:doit(fun foo_noreply_wait/0, []),
    Monitor = monitor(process, Pid),

    Pid ! shoot,
    receive
	{'DOWN', Monitor, process, Pid, Reason} ->
	    case Reason of
		normal ->
		    ok;
		_ ->
		    ct:fail({error_exit_reason, Reason})
	    end
    after 3000 ->
	    ct:fail(timeout)
    end,
    ok.

worker_doit2_reply(Config) ->
    Timeot = ?config(wait_timeout, Config),
    {ok, Pid} = ghost:doit(fun foo_reply/0, []),
    Monitor = monitor(process, Pid),
    receive
	?REPLY_OK ->
	    ok;
	{'DOWN', Monitor, process, Pid, Reason} ->
	   ct:fail({error_exit_before_reply, Reason})
    after Timeot ->
	    ct:fail(timeout)
    end,
    ok.

worker_doit3_reply(Config) ->
    Timeot = ?config(wait_timeout, Config),
    {ok, Pid} = ghost:doit(ghost_SUITE, foo_reply, []),
    Monitor = monitor(process, Pid),
    receive
	?REPLY_OK ->
	    ok;
	{'DOWN', Monitor, process, Pid, Reason} ->
	   ct:fail({error_exit_before_reply, Reason})
    after Timeot ->
	    ct:fail(timeout)
    end,
    ok.




