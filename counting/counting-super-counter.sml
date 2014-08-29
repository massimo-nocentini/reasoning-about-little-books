
signature COUNTER_ITERATOR =
    sig
        structure UnderCounting : UNDER_COUNTING
        val super_counter : 'a UnderCounting.fuc -> int -> int
    end

functor CounterIterator (
    structure UnderCounting : UNDER_COUNTING)
    :> COUNTER_ITERATOR
    =
    struct

    structure UnderCounting = UnderCounting
    
    fun super_counter function n =  
        let fun sc (zero as 0) = function zero
            |   sc  n = let val _ = function n
                        in sc (n-1) end

            val _ = sc n
        in UnderCounting.get_counter () end
    end
