# @version ^0.3.5

enum StateType:
    AVAILABLE
    PLACED
    ACCEPTED

state: public(StateType)

instance_owner: public(address)
asking_price: public(uint256)
# not used, future improvement
description: public(String[100])

instance_buyer: public(address)
offer_price: public(uint256)

@external
def __init__(description_param: String[100], price_param: uint256):
    self.instance_owner = msg.sender
    self.asking_price = price_param
    self.description = description_param
    self.state = StateType.AVAILABLE


@external
def set_offer_local(offer_price_param: uint256):
    # passes if its higher than zero
    assert 0 < offer_price_param, "Value too low."
    # passes if the state is available
    assert StateType.AVAILABLE == self.state, "State is unavailable."
    # passes if the sender is not the owner
    assert msg.sender != self.instance_owner , "Owner can not buy the item."
    
    self.instance_buyer = msg.sender
    self.offer_price = offer_price_param
    self.state = StateType.PLACED

@external
def reject_offer():
    # passes if the item has an offer
    assert StateType.PLACED == self.state, "State is not placed."
    # passes if the item owner is the sender
    assert msg.sender == self.instance_owner, "Only owner can reject an offer."
    
    self.instance_buyer = ZERO_ADDRESS
    self.state = StateType.AVAILABLE

@external
def accept_offer():
    # passes if the item has an offer
    assert StateType.PLACED == self.state, "State is not placed."
    # passes if the item owner is the sender
    assert msg.sender == self.instance_owner, "Only owner can accept an offer."
    
    self.state = StateType.ACCEPTED