%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 26 Oct 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_balcony).


-include("balcony.hrl").
-include("state.hrl").


%% API
-export([


	 calc_errors/1,
	 calc_pid/1,
	 activate/1,
	 stop_session/0,
	 
	 get_temp/0,
	 is_available/0,
	 are_temp_sensors_available/0,
	 are_on_off_plugs_available/0,
	 
	 reachable_status/0
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop_session()->
    rd:call(zigbee,call,[?HeatherBalcony,turn_off,[]],5000),
    rd:call(zigbee,call,[?HeatherDoor,turn_off,[]],5000),
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
calc_errors(State)->
    Result=case rd:call(zigbee,call,[?TempSensor,temp,[]],5000) of
	       {error,Reason}->
		   {error,[Reason,?MODULE,?LINE],State};
	       ActualTemp->
		   io:format("ActualTemp ~p~n",[{ActualTemp,?MODULE,?LINE}]), 
		   timer:sleep(3000),
		   NewError=State#state.setpoint-ActualTemp,
		   T1=State#state.total_error+NewError,
		   NewTotalError=if 
				     T1>?MaxControl->
					 ?MaxControl;
				     T1<?MinControl->
					 ?MinControl;
				     true->
					 T1
				 end,
		   {ok,State#state{
			 actual_temp=ActualTemp,
			 error=NewError,
			 total_error=NewTotalError}}
	   end,
    rpc:cast(node(),balcony,calc_errors_result,[Result]).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
calc_pid(State)->
    P= State#state.kp*State#state.error,
    I= (State#state.ki*State#state.pwm_width)*State#state.total_error,
    D= State#state.kd*State#state.error,
    PidValue=P+I+D+State#state.base_offset,
    ActualWidth=trunc(PidValue),
    Result={ok,State#state{
		 p=P,i=I,d=D,
		 pid_value=PidValue,
		 actual_width=ActualWidth}},
    rpc:cast(node(),balcony,calc_pid_result,[Result]).


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
activate(State)->
    ActualWidth=State#state.actual_width,
    PwmWidth=State#state.pwm_width,
    if
	ActualWidth>PwmWidth->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(PwmWidth*1000);
	ActualWidth < 1 ->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep(PwmWidth*1000);
	true ->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(ActualWidth*1000),
	    rd:call(zigbee,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep((PwmWidth-ActualWidth)*1000)
    end,
    NewElapsedTime=State#state.session_elapsed_time+PwmWidth,
    if 
	NewElapsedTime>State#state.max_session_time ->
	    rpc:cast(node(),balcony_pid,stop_session,[]);
	true->
	    NewState=State#state{session_elapsed_time=NewElapsedTime},
	    rpc:cast(node(),balcony,activate_result,[{ok,NewState}])
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
control_loop(SetPoint,_PreviousErrorXXX,TotalError)->

    %% 1) Calculate errors
    %% calc_errors(SetPoint,TotalError)->{NewError,NewTotalError} | {error,Reason}

    NewError=get_error(SetPoint),
    T1=TotalError+NewError,
    NewTotalError=if 
		      T1>?MaxControl->
			  ?MaxControl;
		      T1<?MinControl->
			  ?MinControl;
		      true->
			  T1
		  end,

    %% 2) Calculate new PID values and use them to set active pwm time
    %% calc_pid(NewError,NewTotalError)->[{p,P},{i,I},{d,D},{pidValue,PidValue}]

    P= ?Kp*NewError,
    I= (?Ki*?PwmWidth)*NewTotalError,
 %   D= (?Kd/?PwmWidth)*NewError,
    D=?Kd*NewError,
 
    PidValue=P+I+D+?BaseOffset,
    ActualWidth=trunc(PidValue),
  %  ActualWidth=trunc(PidValue+?BaseOffset),
 
    io:format("NewError ~p~n",[{NewError,?MODULE,?LINE}]),
    io:format("T1 ~p~n",[{T1,?MODULE,?LINE}]),
    io:format("P ~p~n",[{P,?MODULE,?LINE}]),
    io:format("I ~p~n",[{I,?MODULE,?LINE}]),
    io:format("D ~p~n",[{D,?MODULE,?LINE}]),
    io:format("PidValue ~p~n",[{PidValue,?MODULE,?LINE}]),
    io:format("ActualWidth ~p~n",[{ActualWidth,?MODULE,?LINE}]),
 
    %% 3) Control the heathers
    %% activate(ActualWidth)-> ok.

    if
	ActualWidth>?PwmWidth->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(?PwmWidth*1000);
	ActualWidth < 1 ->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep(?PwmWidth*1000);
	true ->
	    rd:call(zigbee,call,[?HeatherBalcony,turn_on,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_on,[]],5000),
	    timer:sleep(ActualWidth*1000),
	    rd:call(zigbee,call,[?HeatherBalcony,turn_off,[]],5000),
	    rd:call(zigbee,call,[?HeatherDoor,turn_off,[]],5000),
	    timer:sleep((?PwmWidth-ActualWidth)*1000)
    end,
    rpc:cast(node(),balcony,control_loop,[NewError,NewTotalError]).


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
new_proportional_value(Kp,NewError)->
    Kp*NewError.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
new_integral_value(IntegralValue,Ki,NewError,DeltaTime)->
    Ki*((IntegralValue+NewError)).
 %   Ki*((IntegralValue+NewError)*DeltaTime).
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
new_derivate_value(Kd,PreviousError,NewError,DeltaTime)->
   (PreviousError-NewError)/DeltaTime.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_error(SetPoint)->
  SetPoint-get_temp().
    
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_temp()->    
    rd:call(zigbee,call,[?TempSensor,temp,[]],5000).



    
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
are_temp_sensors_available()->    
    IsReachable=rd:call(zigbee,call,[?TempSensor,is_reachable,[]],5000).
        
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
are_on_off_plugs_available()->    
    HB=rd:call(zigbee,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee,call,[?HeatherDoor,is_reachable,[]],5000),
    case {HB,HD} of
	{true,true}->
	    true;
	_->
	    false
    end.
    
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
is_available()->    
    HB=rd:call(zigbee,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee,call,[?HeatherDoor,is_reachable,[]],5000),
    case {HB,HD} of
	{true,true}->
	    true;
	_->
	    false
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
reachable_status()->    
    HB=rd:call(zigbee,call,[?HeatherBalcony,is_reachable,[]],5000),
    HD=rd:call(zigbee,call,[?HeatherDoor,is_reachable,[]],5000),
    [{HB,?HeatherBalcony},{HD,?HeatherDoor}].
%%%===================================================================
%%% Internal functions
%%%===================================================================
