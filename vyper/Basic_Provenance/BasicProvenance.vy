# @version ^0.3.5

enum StateType:
    CREATED
    IN_TRANSIT
    COMPLETED

state: public(StateType)

counter_party: public(address)
previous_counter_party: public(address)

supply_chain_owner: public(address)
# not used
supply_chain_observer: public(address)

@external
def __init__(supply_chain_owner_param: address, supply_chain_observer_param: address):
    self.counter_party = msg.sender

    self.supply_chain_owner = supply_chain_owner_param
    self.supply_chain_observer = supply_chain_observer_param

    self.state = StateType.CREATED

@external
def transfer_responsibility(new_counter_party: address):
    # only the current counter party can transfer responsability and the product still needs to be on the way
    assert self.counter_party == msg.sender and self.state != StateType.COMPLETED, "Either the transfer is done or the caller is not the current counter party."
    
    if self.state == StateType.CREATED:
        self.state = StateType.IN_TRANSIT

    self.previous_counter_party = self.counter_party
    self.counter_party = new_counter_party

@external
def complete():
    # only the owner can complete and the responsabilty needed to be transfer at least once
    assert self.supply_chain_owner == msg.sender and self.state == StateType.IN_TRANSIT, "Revert"
    
    self.state = StateType.COMPLETED
    self.previous_counter_party = self.counter_party
    self.counter_party = empty(address)