  
module 0x1::auction { 
    use Std::Signer; 

    const ERR_CAN_NOT_BID: u64 = 101; 
    const ERR_AUCTION_NOT_LIVE: u64 = 102;
    const ERR_BID_TOO_LOW: u64 = 103;
    const ERR_SAME_HIGHEST_BIDDER: u64 = 104;   
    const ERR_CAN_STILL_BID: u64 = 105;    
    const ERR_ONLY_BENE_CAN_END: u64 = 106;          
 
    struct Auction has key, drop, store { 
        beneficiary: address,
        highestBidder: address,
        highestBid: u64,
        pendingReturns: u64,
        pendingBidder: address,
        canBid: bool,
        ended: bool,
    }

    struct Wallet has key, drop, store { 
        balance: u64,
    } 

    public fun init_Auction( 
        account: &signer, 
    ) { 
        
        let owner_address = Signer::address_of(account);

        move_to(account, Auction { 
               beneficiary: owner_address,
               highestBidder: owner_address,
               highestBid: 0,
               pendingReturns: 0,
               pendingBidder: owner_address,
               canBid: true,
               ended: false
            } 
        ); 
    } 

    public fun bid( 
        bidder: &signer, 
        bid: u64, 
    ) acquires Auction, Wallet {
        let auctionConfig = borrow_global_mut<Auction>(@auction); 
        let acc_bidder = Signer::address_of(bidder);

        assert!(!auctionConfig.canBid, ERR_CAN_NOT_BID);
        assert!(auctionConfig.ended, ERR_AUCTION_NOT_LIVE);
        assert!(auctionConfig.highestBid > bid, ERR_BID_TOO_LOW);
        assert!(auctionConfig.highestBidder == acc_bidder, ERR_SAME_HIGHEST_BIDDER);
        
        auctionConfig.pendingReturns = auctionConfig.highestBid;
        auctionConfig.highestBid = bid;
        auctionConfig.pendingBidder = auctionConfig.highestBidder;
        auctionConfig.highestBidder = acc_bidder;
        auctionConfig.canBid = false;   

        let wallet = borrow_global_mut<Wallet>(acc_bidder);  
        let local_current_balance = wallet.balance;
        
        wallet.balance = local_current_balance - bid;
    }

    public fun withdraw( 
    ) acquires Auction, Wallet {
        let auctionConfig = borrow_global_mut<Auction>(@auction); 
        
        assert!(auctionConfig.canBid, ERR_CAN_STILL_BID);
        assert!(auctionConfig.ended, ERR_AUCTION_NOT_LIVE);
        
        let wallet = borrow_global_mut<Wallet>(auctionConfig.pendingBidder);  
        let local_current_balance = wallet.balance;        
        wallet.balance = local_current_balance + auctionConfig.pendingReturns;

        auctionConfig.pendingReturns = 0;
        auctionConfig.canBid = true;                
    }

    public fun endAuction( 
        sender: &signer,
    ) acquires Auction, Wallet {
        let auctionConfig = borrow_global_mut<Auction>(@auction); 
        let acc_sender = Signer::address_of(sender);

        assert!(auctionConfig.ended, ERR_AUCTION_NOT_LIVE);
        assert(auctionConfig.beneficiary != acc_sender, ERR_ONLY_BENE_CAN_END);
        
        let wallet = borrow_global_mut<Wallet>(auctionConfig.beneficiary);  
        let local_current_balance = wallet.balance;        
        wallet.balance = local_current_balance + auctionConfig.highestBid;

        auctionConfig.pendingReturns = 0;
        auctionConfig.ended = true;                
    }

 
}