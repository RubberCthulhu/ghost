%%%-------------------------------------------------------------------
%%% @author KernelPanic <alevandal@kernelpanic.svyazcom.ru>
%%% @copyright (C) 2012, KernelPanic
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by KernelPanic <alevandal@kernelpanic.svyazcom.ru>
%%%-------------------------------------------------------------------
-module(ghost).

-vsn("0.3.0").

-behaviour(gen_server).

%% API
-export([start/0, start/1, stop/0, start_link/2, doit/1, doit/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	  state = start :: start | work | finish,
	  from :: pid(),
	  callback_spec :: {atom(), atom(), [term()]}
}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(ghost).

start(Type) ->
    application:start(ghost, Type).

stop() ->
    application:stop(ghost).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(From, CallbackSpec) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(From, CallbackSpec) ->
    gen_server:start_link(?MODULE, [From, CallbackSpec], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([From, CallbackSpec]) ->
    Result = case CallbackSpec of
		 {_Module, _Func, Args} when is_list(Args) ->
		     State = #state{
		       state = start, 
		       from = From, 
		       callback_spec = CallbackSpec
		      },
		     {ok, State, 0};
		 _ ->
		     {stop, {error, badarg}}
	     end,
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{state = start, from = From, callback_spec = CallbackSpec}) ->
    {Module, Func, Args} = CallbackSpec,
    Result = apply(Module, Func, Args),
    case Result of
	{reply, Reply} ->
	    reply(From, Reply);
	noreply ->
	    ok;
	_Error ->
	    ok % I need to sleep on it...
    end,
    {stop, normal, State#state{state = finish}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doit(CallbackSpec = {_Module, _Func, Args}) when is_list(Args) ->
    case ghost_sup:start_child(self(), CallbackSpec) of
	{ok, _Pid} ->
	    ok;
	Error ->
	    Error
    end;
doit(_) ->
    {error, badarg}.

doit(Module, Func, Args) ->
    doit({Module, Func, Args}).

reply(Ref, Reply) ->
    gen_server:cast(Ref, Reply).
