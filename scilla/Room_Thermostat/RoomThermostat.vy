scilla_version 0

import BoolUtils

library RoomThermostat

let fail_code     = Uint32 1
let success_code  = Uint32 2

type StateType =
  | Created
  | InUse

type ModeType =
  | Off
  | Cool
  | Heat
  | Auto

let zero = Uint32 0
let target_Temp = Uint32 0

contract RoomThermostat
 (
  thermostat_user     : ByStr20,
  thermostat_installer: ByStr20
)

field state: StateType = Created

field user        : ByStr20 = thermostat_user
field installer   : ByStr20 = thermostat_installer
field target_Temp : Uint32  = target_Temp

field mode : ModeType = Off

transition start_Thermostat ()
 l_installer <- installer;
 is_installer = builtin eq l_installer _sender;
  match is_installer with
   | False =>
    e = {_eventname : "start_Thermostat"; code : fail_code};
    event e
   | True => 
    l_State <- state;
    match l_State with
      | Created =>
        l_Result_State = InUse;
        state := l_Result_State;
        e = {_eventname : "start_Thermostat"; code : success_code};
        event e
      | _ =>
        e = {_eventname : "start_Thermostat"; code : fail_code};
        event e
    end
  end
end

transition set_Target_Temperature (temperature : Uint32)
 l_user <- user;
 is_User = builtin eq l_user _sender;
  match is_User with
   | True => 
    l_state <- state;
     match l_state with
      | Created => 
       e = {_eventname : "set_Target_Temperature"; code : fail_code};
       event e
      | InUse =>
       target_Temp := temperature;
       e = {_eventname : "set_Target_Temperature"; code : success_code};
       event e
     end
   | False =>
    e = {_eventname : "set_Target_Temperature"; code : fail_code};
    event e
  end
end

transition set_Mode (mode_Target : ModeType)
 l_user <- user;
 is_User = builtin eq l_user _sender;
  match is_User with
   | False =>
    e = {_eventname : "set_Mode"; code : fail_code};
    event e
   | True => 
    l_state <- state;
     match l_state with
      | Created => 
       e = {_eventname : "set_Mode"; code : fail_code};
       event e
      | InUse =>
       mode := mode_Target;
       e = {_eventname : "set_Mode"; code : success_code};
       event e
     end
  end
end