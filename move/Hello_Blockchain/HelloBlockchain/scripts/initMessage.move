script {  
    use Std::HelloBlockchain;  
    use Std::Debug;  
 
    fun init(account1: signer) { 
        let m1 = HelloBlockchain::create_message(&account1, b"hello"); 
    } 
} 