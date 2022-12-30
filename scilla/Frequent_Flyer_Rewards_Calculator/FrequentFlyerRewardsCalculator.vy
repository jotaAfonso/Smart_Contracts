scilla_version 0

import BoolUtils IntUtils ListUtils

library FrequentFlyerRewardsCalculator

let fail_code     = Uint32 1
let success_code  = Uint32 2

type StateType =
  | Flyer_Reward
  | Miles_Added

let miles_Values_Length = Uint32 10
let zero                = Uint32 0
let one                 = Uint32 1

contract FrequentFlyerRewardsCalculator
 (
  airline_param     : ByStr20,
  flyer_param       : ByStr20,
  rewards_Per_Mile  : Uint32
)

field state: StateType = Flyer_Reward

field flyer       : ByStr20 = flyer_param
field airline_rep : ByStr20 = airline_param

field miles               : List Uint32 = Nil {(Uint32)}
field miles_current_index : Uint32 = Uint32 0
field total_Rewards       : Uint32 = Uint32 0
field miles_Counter       : Uint32 = Uint32 0

procedure compute_Total_Rewards (miles_param: Uint32)
 l_total_Rewards <- total_Rewards;
 local_v = builtin add l_total_Rewards miles_param;
 total_Rewards := local_v;
 e = {_eventname : "compute_Total_Rewards"; code : success_code};
 event e
end

transition add_Miles (miles_param: Uint32)
 l_flyer <- flyer;
 is_flyer = builtin eq l_flyer _sender;
  match is_flyer with
   | False =>
    e = {_eventname : "add_Miles"; code : fail_code};
    event e
   | True => 
    l_State <- state;
     match l_State with
      | _ =>
       l_miles <- miles;
       (* missing adding to list *)
  	   miles := l_miles ;
       
  	   forall l_miles compute_Total_Rewards;
  	   l_Result_State = Miles_Added;
       state := l_Result_State;
       
       t_r <- total_Rewards;
       e = {_eventname : "add_Miles_total"; code : success_code; total: t_r};
       event e
    end
  end
end