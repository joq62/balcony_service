%----------------- For Testing 
-define(MainNodes,['main_a@c200','main_a@c201','main_a@c202','main_a@c230']).

%%---------- Log
-define(MainLogDir,"logs").
-define(LocalLogDir,"log.logs").
-define(LogFile,"test_logfile").
-define(MaxNumFiles,10).
-define(MaxNumBytes,100000).


%-------------------------------------------------




-define(SetPoint,21).

-define(MaxSessionTime,1000*60*60*6). %% In seconds
%-define(MaxSessionTime,60*4). %% In seconds
-define(SampleInterval,60*1000).
%-define(DeltaTime,?SampleInterval).
%-define(PwmWidth,2*60).
-define(PwmWidth,40).  %% in seconds
-define(DeltaTime,?PwmWidth).
-define(Kp,0.1).
-define(Ki,0.002).
-define(Kd,6).
-define(MaxControl,0.5*?PwmWidth).
-define(MinControl,-?MaxControl).

-define(TempSensor,"weather_1").
-define(HeatherBalcony,"switch_inglasade_heather_balcony").
-define(HeatherDoor,"switch_inglasade_heather_door").

-define(MaxTempDiff,6).
-define(BaseOffset,0.5*?PwmWidth).
