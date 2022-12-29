# @version ^0.3.5

interface Bazaar:
    def has_Balance(instance_buyer: address, item_price: int128) -> bool: view
    def update_Balance(balance_to_update: address, item_price: int128): nonpayable

interface Workbench:
    def contract_Created(): nonpayable
    def contract_Updated(action: String[100]): nonpayable

enum StateType:
    Available
    Sold

party_A: public(address)
party_B: public(address)

instance_seller: public(address)
instance_buyer: public(address)

item_name: public(String[100])
item_price: public(int128)

state: public(StateType)

parent_contract: address

logger: Workbench

@external
def __init__(seller: address, parent_contract_address: address, item_name_param: String[100],
 item_price_param: int128, party_A_param: address, party_B_param: address):
    
    self.party_A = party_A_param
    self.party_B = party_B_param

    self.instance_seller = seller
    self.parent_contract = parent_contract_address
    
    self.item_name = item_name_param
    self.item_price = item_price_param

    self.state = StateType.Available

    self.logger = Workbench(self)
    self.logger.contract_Created()    

@external 
def BuyItem():
    self.instance_buyer = msg.sender
    assert self.instance_seller != self.instance_buyer, "Seller can not be the buyer."
    assert msg.sender == self.party_A or msg.sender == self.party_B, "Buyer needs to be one of the two parties involved." 

    local_Bazaar: Bazaar = Bazaar(create_copy_of(self.parent_contract)) 
    assert local_Bazaar.has_Balance(self.instance_seller, self.item_price), "Does not have enough balance."
    local_Bazaar.update_Balance(self.instance_seller, self.item_price)
    local_Bazaar.update_Balance(self.instance_buyer, -self.item_price)

    self.state = StateType.Sold
    self.logger = Workbench(self.parent_contract)
    self.logger.contract_Updated("BuyItem")
