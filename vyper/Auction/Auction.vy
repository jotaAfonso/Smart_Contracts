# @version ^0.3.5

enum StateType:
    Start
    Bid
    Closed

beneficiary: public(address)

highestBidder: public(address)
highestBid: public(uint256)

pendingReturns: public(HashMap[address, uint256])
state: public(StateType)

@external
def __init__():
    self.beneficiary = msg.sender
    self.state = StateType.Start

@external
@payable 
def bid():
    # not using ands to define every assert
    assert StateType.Start == self.state, "Not able to place a bid."
    assert msg.sender != self.highestBidder, "Bidder can not bid while still being the highest bidder."
    assert msg.sender != self.beneficiary, "Bidder can be the beneficiary."
    assert msg.value > self.highestBid, "Invalid bid, due to low value of the bid."

    # save previous highest bid funds
    self.pendingReturns[self.highestBidder] += self.highestBid

    # update highest bid, address and value
    self.highestBidder = msg.sender
    self.highestBid = msg.value
    
    # update contract state
    self.state = StateType.Bid

@external
def withdraw():
    assert self.state != StateType.Start, "Can only withdraw after a bid." 

    # Checks if the msg sender has pending returns
    pending_amount: uint256 = self.pendingReturns[msg.sender]
    assert pending_amount > empty(uint256), "Msg sender does not have pending returns." 

    # update contract state and pending returns
    self.pendingReturns[msg.sender] =  empty(uint256)
    self.state = StateType.Start

    # returns pending returns to user
    send(msg.sender, pending_amount)
    
@external
def end_Auction():
    assert self.state != StateType.Closed, "Auction already ended."
    assert self.beneficiary == msg.sender, "Only the auction beneficiary can end the auction itself."

    # update contract state
    self.state = StateType.Closed

    # beneficiary receives highest bid value
    send(self.beneficiary, self.highestBid)

@external
def is_Winner() -> bool:
    assert self.state == StateType.Closed, "Auction still live."
    
    return self.highestBidder == msg.sender