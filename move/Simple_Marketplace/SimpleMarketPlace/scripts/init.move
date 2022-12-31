script {   
    use Std::SimpleMarketPlace;   
    use Std::Debug;   
  
    fun init(account1: signer) {  
        let m1 = SimpleMarketPlace::create_item(&account1, b"Nome Teste", 5); 
        Debug::print(&m1);         
    }  
} 