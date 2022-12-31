// modules/hello_world.move 
module 0x1::HelloBlockchain { 

    use Std::Signer;
    use Std::Vector; 

    const ERR_MESSAGE_EXISTS: u64 = 100;  
    
    const ERR_MESSAGE_NOT_EXISTS: u64 = 101;   

    struct Message has store, copy, drop, key {
        message_text: vector<u8>
    }

    public fun create_message(account: &signer, message_param: vector<u8>): vector<u8> {
        let acc_addr = Signer::address_of(account);

        assert!(!message_exists(acc_addr), ERR_MESSAGE_EXISTS);

        let local_message = Message {
            message_text: message_param
        }; 

        move_to<Message>(account, local_message);

        message_param
    }

    public fun message_exists(acc_addr: address): bool {
        exists<Message>(acc_addr)
    }

    public fun change_message(account: &signer, message_param: vector<u8>): vector<u8> acquires Message {
        let acc_addr = Signer::address_of(account);

        assert!(message_exists(acc_addr), ERR_MESSAGE_NOT_EXISTS);

        let local_msg = Message { message_text: message_param } ;

        let message_to_change = borrow_global_mut<Message>(acc_addr);

        message_to_change.message_text = local_msg.message_text;

        local_msg.message_text
    }

    public fun send_to_new(account: &signer, account_to: &signer) acquires Message {
        let acc_addr_1 = Signer::address_of(account);
        let acc_addr_2 = Signer::address_of(account_to);
        
        assert!(message_exists(acc_addr_1), ERR_MESSAGE_NOT_EXISTS);

        let message_to_send = borrow_global_mut<Message>(acc_addr_1);

        assert!(!message_exists(acc_addr_2), ERR_MESSAGE_EXISTS);

        let local_message = Message {
            message_text: message_to_send.message_text
        }; 

        move_to<Message>(account_to, local_message);
    }

    public fun send_to_change(account: &signer, account_to: &signer): vector<u8> acquires Message {
        let acc_addr_1 = Signer::address_of(account);
        let acc_addr_2 = Signer::address_of(account_to);
        
        assert!(message_exists(acc_addr_1), ERR_MESSAGE_NOT_EXISTS);

        let message_to_send = borrow_global_mut<Message>(acc_addr_1);

        assert!(message_exists(acc_addr_2), ERR_MESSAGE_NOT_EXISTS);

        let local_msg = Message {
            message_text: message_to_send.message_text
        }; 

        let message_to_change = borrow_global_mut<Message>(acc_addr_2);

        message_to_change.message_text = local_msg.message_text;

        local_msg.message_text
    }

    public fun get_message(account: &signer): vector<u8> acquires Message {
        let acc_addr = Signer::address_of(account);

        borrow_global<Message>(acc_addr).message_text
    }
} 
 
