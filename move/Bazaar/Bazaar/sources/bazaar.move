module 0x1::bazaar { 
    use Std::Signer; 
    use 0x1::itemList;  

    const ERR_HAS_ITEM: u64 = 101; 
    const ERR_HAS_NOT_ITEM: u64 = 102;
    const ERR_WRONG_SELLER: u64 = 103;
    const ERR_NOT_ENOUGH_BALANCE: u64 = 104;   
    const ERR_HAS_WALLET: u64 = 105;       
    const ERR_HAS_NOT_WALLET: u64 = 106;       

    struct Config has key { 
        hasItem: bool, 
    } 

    struct Wallet has key, drop, store { 
        balance: u64, 
    } 
 
    public fun initialize_market( 
        account: &signer, 
    ) { 
        move_to( 
            account, 
            Config { 
               hasItem: false,
            } 
        ); 
    } 

    struct Listing has key, drop, store {
        price: u64,
        name: vector<u8>,
    }

    public fun create_wallet( 
        owner: &signer, 
        balance: u64, 
    ) {
        let acc_addr = Signer::address_of(owner);
        assert!(exists<Wallet>(acc_addr), ERR_HAS_WALLET);
        
        move_to<Wallet>(owner,  Wallet { 
               balance: balance,
            } );
    }
 
    public fun create_listing( 
        owner: &signer, 
        price: u64, 
        name: vector<u8>,
    ) acquires Config { 

        let config = borrow_global_mut<Config>(@marketplace);

        assert!(config.hasItem == true, ERR_HAS_ITEM);

        itemList::create_Item( 
            owner,  
            price, 
            name,
        ); 

        config.hasItem = true
    } 
 
    public fun buy_listing( 
        buyer: &signer, 
        seller: &signer, 
        offer: u64, 
    ) acquires Config, Listing, Wallet { 
        // charge fee for the aggregator 
        let acc_addr_1 = Signer::address_of(buyer);
        let acc_addr_2 = Signer::address_of(seller);
        
        let config = borrow_global_mut<Config>(@marketplace); 

        assert!(config.hasItem == false, ERR_HAS_NOT_ITEM);

        assert!(!exists<Listing>(acc_addr_2), ERR_WRONG_SELLER);

        let listing_local = borrow_global<Listing>(acc_addr_2); 
  
        assert!(listing_local.price > offer, ERR_NOT_ENOUGH_BALANCE);

        assert!(!exists<Wallet>(acc_addr_1), ERR_HAS_NOT_WALLET);
        assert!(!exists<Wallet>(acc_addr_2), ERR_HAS_NOT_WALLET);

        let item = move_from<Listing>(acc_addr_2);      

        let wallet_buyer = borrow_global_mut<Wallet>(acc_addr_1); 

        let local_buyer_b = wallet_buyer.balance; 
        wallet_buyer.balance = local_buyer_b - item.price;

        let wallet_seller = borrow_global_mut<Wallet>(acc_addr_2); 

        let local_seller_b = wallet_seller.balance; 
        wallet_seller.balance = local_seller_b + item.price;

        move_to<Listing>(buyer, item);
    } 
}