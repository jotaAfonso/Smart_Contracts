// modules/hello_world.move   
module 0x1::BasicProvenance {   
    use Std::Signer;  
    use Std::Vector;  
    use Std::Option;  
     
  
    const ERR_RESPONSABILITY_EXISTS: u64 = 100;      
  
    struct Responsability has store, copy, drop, key {  
        owner: address, 
        current_party: address 
    }  
  
    public fun create(account: &signer) {  
 
        let acc_addr = Signer::address_of(account);  
  
        assert!(!responsability_exists(acc_addr), ERR_RESPONSABILITY_EXISTS);  
  
        let local_res = Responsability { 
            owner: acc_addr, 
            current_party: acc_addr 
        }; 
  
        move_to<Responsability>(account, local_res);  
    }  
  
    fun responsability_exists(acc_addr: address): bool {  
        exists<Responsability>(acc_addr)  
    }  
  
    public fun transfer_reponsability(current: &signer, new_party: &signer) acquires Responsability {  
        let acc_addr_1 = Signer::address_of(current); 
        let acc_addr_2 = Signer::address_of(new_party); 
  
        let Responsability { owner, current_party } = move_from<Responsability>(Signer::address_of(current)); 
 
        let local_res = Responsability { 
            owner: owner, 
            current_party: acc_addr_2 
        }; 
 
        move_to<Responsability>(new_party, local_res); 
    }  
}   
   
