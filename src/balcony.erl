%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%%
%%% 
% Kp - proportional gain
% Ki - integral gain
% Kd - derivative gain
% sample_interval - loop interval time
% previous_error := 0
% integral := 0
% loop:
%   error := setpoint − measured_value
%   proportional := error;
%   integral := integral + error × dt
%   derivative := (error − previous_error) / dt
%   output := Kp × proportional + Ki × integral + Kd × derivative
%   previous_error := error
%   wait(dt)
%   goto loop
%   pwm_interval = 50 seconds 
%   dt= 20 sec						%
%   0 <= pwm_value <= pwm_interval  
%
%%--------------------------- API ------------------------------------
%
%
%
%
%%
%%% Created :  2 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(balcony).
 
-behaviour(gen_server).  
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("balcony.rd").
-include("balcony.hrl").
-include("log.api").

%% API

%% Application handling API

-export([
	 new_session/0,
	 stop_session/0,
	 get_temp/0,
	 in_session/0,
	 is_available/0,

	 pid_info/0,
	 calc_errors_result/1,	 
	 calc_pid_result/1,
	 activate_result/1
	]).

%% Debug test API

-export([

%	 all_nodes/0,
%	 all_providers/0,
%	 where_is/1,
%	 is_wanted_state/0
	]).



%% Debug API

-export([start/0,
	 ping/0]).


-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%% Record and Data
-include("state.hrl").


%% Table or Data models


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new session. If a session is ongoing that session is stopped
%% and a new session is started. The PID controller activates and controls
%% the heathers     
%% States: no_session, ongoing_session
%% @end
%%--------------------------------------------------------------------
-spec new_session() -> ok | {error, Error :: term()}.
new_session()  ->
    gen_server:call(?SERVER,{new_session},infinity).

-spec new_session(SetPoint :: integer()) -> ok | {error, Error :: term()}.
new_session(SetPoint)  ->
    gen_server:call(?SERVER,{new_session,SetPoint},infinity).

-spec new_session(SetPoint :: integer(),SessionTime :: integer()) -> ok | {error, Error :: term()}.
new_session(SetPoint,SessionTime)  ->
    gen_server:call(?SERVER,{new_session,SetPoint,SessionTime},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Gets the latest measured temperture
%% 
%% @end
%%--------------------------------------------------------------------
-spec get_temp() -> Temp :: float() | {error, Error :: term()}.
get_temp()->
    gen_server:call(?SERVER,{get_temp},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  Reply if a session is ongoing or not 
%% 
%% @end
%%--------------------------------------------------------------------
-spec in_session() -> InSession :: boolean() | {error, Error :: term()}.
in_session()->
    gen_server:call(?SERVER,{in_session},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  Reply if the balcony heathers are reachable  
%% 
%% @end
%%--------------------------------------------------------------------
-spec is_available() -> IsAvailable :: boolean() | {error, Error :: term()}.
is_available()->
    gen_server:call(?SERVER,{is_available},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Gets pid controllers internal data. Debug purpose
%% 
%% @end
%%--------------------------------------------------------------------
-spec pid_info() -> State :: term() | {error, Error :: term()}.
pid_info()->
    gen_server:call(?SERVER,{pid_info},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Stops a ongoing session. The PID controller deactivates and turns off
%% the heathers. Same procedure if no session is ongoing    
%% @end
%%--------------------------------------------------------------------
-spec stop_session() -> ok.
stop_session()  ->
    gen_server:cast(?SERVER,{stop_session}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec calc_errors_result({Result :: atom(),NewState :: term()}) -> ok.
calc_errors_result({Result,NewState})  ->
    gen_server:cast(?SERVER,{calc_errors_result,{Result,NewState}}).

-spec calc_pid_result({Result :: atom(),NewState :: term()}) -> ok.
calc_pid_result({Result,NewState})  ->
    gen_server:cast(?SERVER,{calc_pid_result,{Result,NewState}}).

-spec activate_result({Result :: atom(),NewState :: term()}) -> ok.
activate_result({Result,NewState})  ->
    gen_server:cast(?SERVER,{activate_result,{Result,NewState}}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->

    {ok, #state{
	    
	   },0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.


handle_call({new_session}, _From, State)->
    NewState=State#state{in_session=true},			 
    spawn(fun()->lib_balcony:calc_errors(NewState) end),
    Reply=ok,
    {reply, Reply, NewState};

handle_call({get_temp}, _From, State)->
    Reply=State#state.actual_temp,
    {reply, Reply, State};

handle_call({in_session}, _From, State)->
    Reply=State#state.in_session,
    {reply, Reply, State};

handle_call({is_available}, _From, State)->
    Reply=State#state.is_available,
    {reply, Reply, State};


handle_call({pid_info}, _From, State)->
    Reply=State,
    {reply, Reply, State};

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    Reply = {error,["Unmatched signal ",Request,?MODULE,?LINE]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.


handle_cast({stop_session}, State)->
    lib_balcony:stop_session(),
    NewState=State#state{in_session=false,
			 session_elapsed_time=0},			 
    {noreply, NewState};

handle_cast({calc_errors_result,{error,_Reason,NewState}}, State) when State#state.in_session =:=true ->
    spawn(fun()->lib_balcony:activate(NewState) end),
    {noreply, NewState};

handle_cast({calc_errors_result,{ok,NewState}}, State) when State#state.in_session =:=true ->
    spawn(fun()->lib_balcony:calc_pid(NewState) end),
    {noreply, NewState};

handle_cast({calc_pid_result,{ok,NewState}}, State) when State#state.in_session =:=true ->
    spawn(fun()->lib_balcony:activate(NewState) end),
    {noreply, NewState};

handle_cast({activate_result,{ok,NewState}}, State) when State#state.in_session =:=true ->
    spawn(fun()->lib_balcony:calc_errors(NewState) end),
    {noreply, NewState};


handle_cast(Request, State) ->
    io:format("unmatched match~p~n",[{Request,?MODULE,?LINE}]), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
  %% Announce to resource_discovery
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    timer:sleep(2000),
    
    %% Ensure that heaters are turned off during when restarting
    lib_balcony:stop_session(),
    AreOnOffPlugsAvailable=case lib_balcony:are_on_off_plugs_available() of
			       {error,Reason}->
				   ?LOG_WARNING("Error when checking availability",Reason),
				   false;
			       false->
				   false;
			       true->
				   true
			   end,

    ActualTemp= case lib_balcony:are_temp_sensors_available() of
		    {error,Reason1}->
			?LOG_WARNING("Can not read temp ",Reason1),
			-273;
		    false->
			-273;
		    true->
			case lib_balcony:get_temp() of
			    {error,Reason2}->
				?LOG_WARNING("Can not read temp ",Reason2),
				-273;
			    Temp->
				Temp
			end
		end,
    
    ?LOG_NOTICE("Server started ",[?MODULE,
				   {are_on_off_plugs_available,AreOnOffPlugsAvailable},
				   {temp,ActualTemp}]),
  
      


NewState=State#state{
 %% Static data
	    max_session_time=?MaxSessionTime,
	    pwm_width=?PwmWidth,
	    base_offset=?BaseOffset,
	    kp=?Kp,
	    ki=?Ki,
	    kd=?Kd,
	    %% Runtime 
	    actual_temp=ActualTemp,
	    in_session=false,
	    is_available=AreOnOffPlugsAvailable,
	    session_elapsed_time=0,
	    setpoint=?SetPoint,
	    error=0,
	    total_error=0,
	    actual_width=0,
	    pid_value=0,
	    p=0,
	    i=0,
	    d=0
	    %% debug
	  },
    {noreply, NewState};

handle_info(Info, State) ->
    ?LOG_WARNING("unmatched match info ",[Info]),
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
