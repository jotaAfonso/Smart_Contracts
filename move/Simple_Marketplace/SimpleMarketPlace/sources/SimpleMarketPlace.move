// modules/hello_world.move  
module 0x1::SimpleMarketPlace {  
    use Std::Signer; 
    use Std::Vector;  
 
    const ERR_ITEM_EXISTS: u64 = 100;   
    const ERR_OFFER_PRICE_LOW: u64 = 101;    
    const ERR_OFFER_EXISTS: u64 = 102;    
    const ERR_OFFER_NOT_EXISTS: u64 = 103;    
 
    struct Item has store, copy, drop, key { 
        item_name: vector<u8>, 
        item_price: u8 
    } 
 
    struct Offer has store, copy, drop, key { 
        offer_price: u8
    } 
 
    public fun create_item(account: &signer, name_param: vector<u8>, price_param: u8): Item { 
        let acc_addr = Signer::address_of(account); 
 
        assert!(!item_exists(acc_addr), ERR_ITEM_EXISTS); 
 
        let local_item = Item { 
            item_name: name_param, 
            item_price: price_param 
        };  
 
        move_to<Item>(account, local_item); 
 
        local_item 
    } 
 
    fun item_exists(acc_addr: address): bool { 
        exists<Item>(acc_addr) 
    } 
 
    public fun make_offer(account_Seller: &signer, offer_param: u8): Offer acquires Item { 
        let acc_addr = Signer::address_of(account_Seller); 
 
        assert!(!offer_exists(acc_addr), ERR_OFFER_EXISTS); 

        let local_item_price = get_item_price(account_Seller);
 
        assert!(offer_param > local_item_price, ERR_OFFER_PRICE_LOW); 
 
        let local_offer = Offer { 
            offer_price: offer_param 
        };  
 
        move_to<Offer>(account_Seller, local_offer); 
 
        local_offer 
    } 
 
    fun offer_exists(acc_addr: address): bool { 
        exists<Offer>(acc_addr) 
    } 

    fun get_item_price(account: &signer): u8 acquires Item {
        let acc_addr = Signer::address_of(account);

        borrow_global<Item>(acc_addr).item_price
    }

    public fun reject_offer(account_Seller: &signer) acquires Offer {
        let acc_addr = Signer::address_of(account_Seller);

        assert!(offer_exists(acc_addr), ERR_OFFER_NOT_EXISTS);

        let offer_to_reject = move_from<Offer>(acc_addr);

        let Offer { offer_price: _ } = offer_to_reject;
    }

    public fun accept_offer(account_Seller: &signer, account_Buyer: &signer) acquires Offer, Item {
        let acc_addr_1 = Signer::address_of(account_Seller);
        let acc_addr_2 = Signer::address_of(account_Buyer);

        assert!(offer_exists(acc_addr_1), ERR_OFFER_NOT_EXISTS);

        let offer_to_accept = move_from<Offer>(acc_addr_1);

        let Offer { offer_price: _ } = offer_to_accept;

        let item_to_accept = move_from<Item>(acc_addr_1);

        //let Item { item_name, item_price } = item_to_accept;
        let local_item = Item { 
            item_name: item_to_accept.item_name,
            item_price: item_to_accept.item_price
        };

        move_to<Item>(account_Buyer, local_item);
    }
}  
  
