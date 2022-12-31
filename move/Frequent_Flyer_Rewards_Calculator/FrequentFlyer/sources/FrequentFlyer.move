module 0x1::FrequentFlyer {  
    use Std::Signer; 
    use Std::Vector; 
    use Std::Option; 
    
 
    const ERR_BOOK_EXISTS: u64 = 100;   
    const ERR_NOT_A_FLYER: u64 = 101;    
    const ERR_OFFER_EXISTS: u64 = 102;    
    const ERR_OFFER_NOT_EXISTS: u64 = 103;    
 
    struct Book has store, copy, drop, key { 
        miles: vector<u8> 
    } 
 
    public fun create_Book(airline: &signer) { 

        let acc_addr = Signer::address_of(airline); 
 
        assert!(!book_exists(acc_addr), ERR_BOOK_EXISTS); 
 
        let local_book = Book {
            miles: Vector::empty<u8>()
        };
 
        move_to<Book>(airline, local_book); 
    } 
 
    fun book_exists(acc_addr: address): bool { 
        exists<Book>(acc_addr) 
    } 
 
    public fun add_miles(flyer: &signer, airline: &signer, miles_param: vector<u8>) acquires Book { 
        let acc_addr_flyer = Signer::address_of(flyer); 
        let acc_addr_airline = Signer::address_of(airline); 
 
        // checks flyer isn't a airline
        assert!(!book_exists(acc_addr_flyer), ERR_NOT_A_FLYER); 

        // TODO check airline isn't a flyer

        let local_book = Book { miles: miles_param } ;

        let book_to_change = borrow_global_mut<Book>(acc_addr_airline);

        book_to_change.miles = local_book.miles;
    } 

    public fun compute_total_rewards(airline: &signer):u8 acquires Book {
        let acc_addr = Signer::address_of(airline); 

        assert!(!book_exists(acc_addr), ERR_NOT_A_FLYER); 

        let book_to_read = borrow_global_mut<Book>(acc_addr);

        let i = 0;
        let x = 0;
        let lenght = Vector::length(&book_to_read.miles);
        while(i < lenght){
            x = x + Vector::pop_back(&mut book_to_read.miles);
            i = i + 1;
        }
        x
    }

    public fun get_miles(airline: &signer): vector<u8> acquires Book {
        let acc_addr = Signer::address_of(airline); 

        assert!(!book_exists(acc_addr), ERR_NOT_A_FLYER); 

        let book_to_read = borrow_global<Book>(acc_addr);

        book_to_read.miles
    } 
}  
  
