 
module 0x1::itemList { 
    use Std::Signer;

    //
    // Errors
    //

    const ELISTING_ZERO_TOKEN: u64 = 5;

    struct Listing has key, drop, store {
        price: u64,
        name: vector<u8>,
    }

    //
    // public functions
    //

    public fun create_listing(
        price_Param: u64,
        name_Param: vector<u8>,
    ): Listing {
        assert!(price_Param > 0, ELISTING_ZERO_TOKEN);
        let localItem = Listing {
            price: price_Param,
            name: name_Param
        };

        localItem
    }

    public fun create_Item(
        owner: &signer,
        price: u64,
        name: vector<u8>,
    ) {
        let item = create_listing(
            price,
            name,
        );
       
        move_to<Listing>(owner,item)
    }
}