
signature Y_COMBINATOR = 
    sig

        val Y : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

    end
