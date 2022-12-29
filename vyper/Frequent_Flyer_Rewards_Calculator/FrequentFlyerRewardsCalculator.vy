# @version ^0.3.5

enum StateType:
    FLYER_REWARD
    MILES_ADDED

ZERO: constant(uint256) = 0

state: public(StateType)

airline_rep: public(address)
flyer: public(address)

rewards_per_mile: public(uint256) 
miles: public(DynArray[uint256, 100]) 
miles_current_index: public(uint256)
total_rewards: public(uint256)
miles_counter: public(uint256)

@external
def __init__(flyer_param: address, rewards_per_mile_param: int128):
    self.airline_rep = msg.sender
    self.flyer = flyer_param

    self.rewards_per_mile = convert(rewards_per_mile_param, uint256) 
    self.miles = []
    self.miles_current_index = ZERO
    self.total_rewards = ZERO
    self.miles_counter = ZERO

    self.state = StateType.FLYER_REWARD

@external
def add_miles(miles_param: DynArray[uint256, 50]):
    # only flyer can add miles
    assert self.flyer == msg.sender, "Only flyer can add miles."
    # checks the length of the stored miles with the incoming ones, only proceeds if the is space to add them
    assert len(miles_param) + self.miles_counter <= len(self.miles), "Reached the max capacity for miles added."

    self.miles_counter += len(miles_param)
    for mileLoop in miles_param:
        self.miles.append(mileLoop)

    self.compute_total_rewards()
    self.state = StateType.MILES_ADDED

@internal
def compute_total_rewards() -> uint256:
    for i in range(self.miles_current_index, self.miles_current_index + 100):
        if self.miles_counter == self.miles_current_index:
            break
        self.total_rewards += self.rewards_per_mile * self.miles[i]
        self.miles_current_index += 1 
    
    return self.total_rewards   

@external
def get_miles() -> DynArray[uint256, 100]:
    # flyer needs to add miles at least once
    assert self.state == StateType.MILES_ADDED, "No miles were added."
    return self.miles