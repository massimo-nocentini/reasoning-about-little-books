
signature ARIETIES_IMPERATIVE = 
    sig
        (* 
         We do not provide zero arg since in a recursive function
         at least one argument we suppose will change 
         *)
        val one_arg : ('a -> 'b) ref -> 'a -> 'b
        val two_args : ('a -> 'b -> 'c) ref -> 'a -> 'b -> 'c
        val three_args : ('a -> 'b -> 'c -> 'd) ref -> 'a -> 'b -> 'c -> 'd 
    end

functor ArietiesImperative ()
    :> ARIETIES_IMPERATIVE
    =
    struct

    fun one_arg f arg = !f arg
    fun two_args f arg arg1 = !f arg arg1
    fun three_args f arg arg1 arg2 = !f arg arg1 arg2

    end
