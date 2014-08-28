

signature Y_COMBINATOR_APPLICATIVE = 
    sig
        val Y : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    end

functor Y_applicative_one_arg_from_little_lisper (
    structure SelfApplication : SELF_APPLICATION)
    :> Y_COMBINATOR_APPLICATIVE
    =
    struct

    local open SelfApplication in
        val Y =  fn R => 
                    (fn (hukairs as (Into _)) => R (fn arg => self_apply_one_arg hukairs arg)) 
                        (Into (fn (hukairs as (Into _)) => R (fn arg => self_apply_one_arg hukairs arg)))
    end

    end

signature Y_COMBINATOR_APPLICATIVE_MULTIARGS = 
    sig
        type 'a self
        val Y_multi_args : ('function_type self -> 'b) -> 
                            ('b -> 'function_type) -> 'function_type
    end

functor Y_applicative_multiargs_from_little_lisper (
    structure SelfApplication : SELF_APPLICATION)
    :> Y_COMBINATOR_APPLICATIVE_MULTIARGS where type 'a self = 'a SelfApplication.self 
    =
    struct

    type 'a self = 'a SelfApplication.self

    local open SelfApplication in
        val Y_multi_args =  fn self_apply =>
                                fn R => 
                                    (fn (hukairs as (Into _)) => R (self_apply hukairs) ) 
                                        (Into (fn (hukairs as (Into _)) => R (self_apply hukairs) ))
    end

    end

        
