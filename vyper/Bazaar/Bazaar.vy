# @version ^0.3.5

interface Workbench:
    def contract_Created(): nonpayable
    def contract_Updated(action: String[100]): nonpayable

enum StateType:
    Provisioned
    Listed
    Finalized

state: public(StateType)

party_A: public(address)
party_A_Balance: public(int128)

party_B: public(address)
party_B_Balance: public(int128)

bazaar_maintainer: public(address)

item_listing_template: address

latest_item: public(address)

logger: Workbench

@external
def __init__(party_A_param: address, balance_A_param: int128, party_B_param: address, balance_B_param: int128, item_listing_template_param: address):
    assert party_A_param != party_B_param, "Parties cannot be the same."

    self.bazaar_maintainer = msg.sender

    self.party_A = party_A_param
    self.party_A_Balance = balance_A_param

    self.party_B = party_B_param
    self.party_B_Balance = balance_B_param

    self.item_listing_template = item_listing_template_param

    self.state = StateType.Provisioned

    self.logger = Workbench(self)
    self.logger.contract_Created()   

@external
def list_Item(item_name_param: String[100], item_price_param: int128):
    assert msg.sender == self.party_A or msg.sender == self.party_B, "Seller needs to be one of the two parties involved." 
    self.latest_item = create_from_blueprint(self.item_listing_template, msg.sender, self, item_name_param, item_price_param, self.party_A, self.party_B, code_offset=1)
    
    self.state = StateType.Listed
    self.logger.contract_Updated("List Item.") 

@external
def has_Balance(buyer: address, item_price_param: int128) -> bool:
    assert buyer == self.party_A or buyer == self.party_B, "Buyer needs to be one of the two parties involved." 
    
    if buyer == self.party_A:
        return self.party_A_Balance >= item_price_param

    return self.party_B_Balance >= item_price_param
    
@internal
def add_Balance(party: address, balance_change_param: int128):
    assert party == self.party_A or party == self.party_B, "Party input needs to be one of the two parties involved." 

    # cause im using variables as balance 
    if party == self.party_A:
        self.party_A_Balance += balance_change_param
    if party == self.party_B:
        self.party_B_Balance += balance_change_param

@external
def update_Balance(seller: address, buyer: address, item_price_param: int128):
    self.add_Balance(seller, item_price_param)
    self.add_Balance(buyer, -item_price_param)

    self.state = StateType.Finalized
    self.logger.contract_Updated("Update Balance.") 