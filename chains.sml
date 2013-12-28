structure ChainLittleSchemer = 
    struct

    datatype 'a chain = Link of 'a * ('a -> 'a chain)

    fun chain_maker next = 
	let 
	    fun sequence element = Link ((next element), sequence)
	in sequence end

    fun get_nth nth next =
	let
	    fun N 1 (Link (element, _))= element
	      | N n (Link (element, next_link)) = N (n-1) (next_link element)
	in
	    (N nth) o (chain_maker next)
	end

    fun until pred next =
	let
	    fun P (Link (element, next_link)) = 
		if pred element
		then element
		else P (next_link element)
	in
	    P o (chain_maker next)
	end
	 
    (* using those functions the client code looks more clear and elegant,
     respect the more abstract functions above. *)
    fun ints n = Link (n+1, ints)

    fun chain_item 1 (Link (element, _))= element
      | chain_item n (Link (element, next_link)) = 
	chain_item (n-1) (next_link element)

    fun fibs n m = Link (n+m, fibs m)



    end
