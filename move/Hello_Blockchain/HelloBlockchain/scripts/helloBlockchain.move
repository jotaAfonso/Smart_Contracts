// scripts/run_hello.move 
script { 
    use Std::HelloBlockchain; 
    use Std::Debug; 
 
    fun main(account1: signer, account2: signer) { 
        
        let m1 = HelloBlockchain::change_message(&account1, b" Bolas");
        Debug::print(&m1);
        let m2 = HelloBlockchain::change_message(&account2, b" Amarelas");
        Debug::print(&m2);
        let m3 = HelloBlockchain::get_message(&account2);
        Debug::print(&m3);
        let m4 = HelloBlockchain::send_to_change(&account1, &account2);
        Debug::print(&m4);
        m3 = HelloBlockchain::get_message(&account2);
        Debug::print(&m3);
    } 
} 
