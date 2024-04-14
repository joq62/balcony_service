
%% Record and Data
-record(state,{
	       %% Static data
	       max_session_time,
	       pwm_width,
	       base_offset,
	       kp,
	       ki,
	       kd,
	       %% Runtime 
	       actual_temp,
	       in_session,
	       is_available,
	       session_elapsed_time,
	       setpoint,
	       error,
	       total_error,
	       actual_width,
	       pid_value,
	       p,
	       i,
	       d
	       
	      }).
