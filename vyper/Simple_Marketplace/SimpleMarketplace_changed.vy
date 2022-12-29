# @version ^0.3.5

enum StateType:
    Started
    Bidding
    Withdrawing # (same state as Started but to simplify)
    AuctionClosed

beneficiary: public(address)

highestBidder: public(address)
highestBid: public(uint256)
pendingReturns: public(HashMap[address, uint256])
ended: public(bool)
state: public(StateType)

@external
def __init__(_beneficiary: address):
    self.beneficiary = _beneficiary
    self.state = StateType.Started

@external
@payable # Function is able to receive Ether
def bid():
    assert self.state == StateType.Started or self.state == StateType.Withdrawing, "Invalid State."
    assert self.highestBidder != msg.sender, "Cant bid while being the highest bidder."
    assert not self.ended, "Auction not live."
    assert msg.value > self.highestBid, "Invalid bid, due to low value."

    self.pendingReturns[self.highestBidder] += self.highestBid
    self.highestBid = msg.value
    self.highestBidder = msg.sender
    self.state = StateType.Bidding

@external
def withdraw():
    assert self.state == StateType.Bidding, "Invalid State."
    pending_amount: uint256 = self.pendingReturns[msg.sender]
    self.pendingReturns[msg.sender] = 0
    self.state = StateType.Withdrawing
    send(msg.sender, pending_amount)
    
@external
def endAuction():
    assert not self.ended, "Auction not live"
    assert self.beneficiary == msg.sender, "Only beneficiary can end the Auction."

    self.ended = True
    self.state = StateType.AuctionClosed
    send(self.beneficiary, self.highestBid)

@external
def WinnerOfAuction() -> bool:
    assert self.ended, "Auction still live."
    assert self.state == StateType.AuctionClosed, "Invalid State."
    
    if self.highestBidder == msg.sender:  
        return True
    else:
        return False
