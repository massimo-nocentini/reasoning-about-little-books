
signature Y_COMBINATOR_IMPERATIVE = 
    sig
        val Y_bang : 'a -> (('b -> 'a) -> 'b -> 'a) -> 'b -> 'a 
    end    

signature Y_COMBINATOR_IMPERATIVE_DERIVATION = 
    sig
        include Y_COMBINATOR_IMPERATIVE

        type 'a sexp
        val length : 'a sexp -> int
        val length' : 'a sexp -> int
        val length'' : 'a sexp -> int
        val length''' : 'a sexp -> int
    end

functor Y_imperative_derivation (
    structure Sexp : SEXP)
    :> Y_COMBINATOR_IMPERATIVE_DERIVATION where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp
    
    local open Sexp in

        fun length (List slist) =
            let fun L Null = 0
                |   L (Cons (_, cdr_slist)) = 1 + L cdr_slist
            in L slist end

        fun length' (List slist) = 
            let val L' = 
                    (* 
                     The following commented line of code does produce the
                     following type checking error that I do not understand:
                        let val h_ref : ('a Sexp.slist -> int) ref = ref (fn _ => 0) 
                     >> Error: explicit type variable cannot be generalized at its binding declaration: 'a 
                     With this attempt we would like to fix the type of `h_ref' 
                     just for ease the derivation.
                     *)
                    let val h_ref = ref (fn _ => 0) 
                        val _ = h_ref := (  fn  Null => 0
                                            |   Cons (_, cdr_slist) => 1 + !h_ref cdr_slist)
                    in !h_ref end
            in L' slist end
            
        fun length'' (List slist) = 
            let val L'' =
                let val h_ref = ref (fn _ => 0) 
                    val _ = h_ref := ((fn length => 
                                        fn  Null => 0
                                        |   Cons (_, cdr_slist) => 1 + length cdr_slist) (fn arg => !h_ref arg))
                                        (*|   Cons (_, cdr_slist) => 1 + length cdr_slist) (!h_ref))*)
                in !h_ref end
            in L'' slist end
                 
        fun length''' (List slist) = 
            let val L''' =
                let val h_ref = ref (fn _ => 0) 
                    val L = fn length =>   
                                fn  Null => 0
                                |   Cons (_, cdr_slist) => 1 + length cdr_slist
                    (*val _ = h_ref := (L (!h_ref)) *)
                    val _ = h_ref := (L (fn arg => !h_ref arg))
                in !h_ref end
            in L''' slist end

    end (* ending: `local open Sexp in' *)

    fun Y_bang initial_value L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (fn arg => !h_ref arg))
        in !h_ref end

    (* 
     The following should be an attempt to translate Y-bang, page 123 ,
     but it doesn't work since `val rec' require a function declaration
     on its right hand side.
     *)
    (*fun Y_bang f = let val rec h = (fn f => f (fn arg => h arg))  in h f end*)
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
(*
structure ImperativeY = Y_imperative_one_arg (
    type codomain = int 
    val initial_value = 0)

structure ImperativeMultiArgsY  = Y_imperative_multiargs (
    type codomain = int 
    val initial_value = 0)
*)
