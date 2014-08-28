

signature Y_COMBINATOR_APPLICATIVE = 
    sig
        val Y : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    end
