script {    
    use Std::SimpleMarketPlace;    
    use Std::Debug;    
   
    fun main(seller: signer, buyer: signer) {   
        let m1 = SimpleMarketPlace::make_offer(&seller, 10); 
        //SimpleMarketPlace::reject_offer(&seller); 
        //SimpleMarketPlace::accept_offer(&seller, &buyer);
        Debug::print(&m1);          
    }   
} 