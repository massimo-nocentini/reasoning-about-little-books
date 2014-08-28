
signature Y_COMBINATOR_IMPERATIVE = 
    sig
        val Y_bang : 'a -> (('b -> 'a) -> 'b -> 'a) -> 'b -> 'a 
    end    

functor Y_imperative_one_arg ()
    :> Y_COMBINATOR_IMPERATIVE
    =
    struct

    fun Y_bang initial_value L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (fn arg => !h_ref arg))
        in !h_ref end
    
    end


signature Y_COMBINATOR_IMPERATIVE_MULTIARGS = 
    sig
        structure Ariety : ARIETIES_IMPERATIVE
        val Y_bang_multi_args : ('function_type ref -> 'b) ->
                                'function_type -> ('b -> 'function_type) -> 'function_type
    end

functor Y_imperative_multiargs ()
    :> Y_COMBINATOR_IMPERATIVE_MULTIARGS
    =
    struct

    structure Ariety = ArietiesImperative()

    fun Y_bang_multi_args G initial_thunk L =
        let val h_ref = ref initial_thunk 
            val _ = h_ref := (L (G h_ref))
            (*val _ = h_ref := (L (G (!h_ref)))*)
        in !h_ref end
     

    end
