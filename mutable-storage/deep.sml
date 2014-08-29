
signature DEEP = 
    sig
        type 'a sexp
        val deep : 'a -> int -> { result : 'a sexp, memo_table : (int * 'a sexp) list }
    end

signature COUNTER = 
    sig
        val get_counter: unit -> int
        val set_counter: int -> unit
    end

signature SEXP_CONS_CTOR_COUNTING = 
    sig
        include SEXP_CONS_CTOR
        include COUNTER
    end

functor SexpConsCtorCounting (
    structure Sexp : SEXP)
    :> SEXP_CONS_CTOR_COUNTING  where type 'a sexp = 'a Sexp.sexp
                                where type 'a slist = 'a Sexp.slist
    =
    struct

    type 'a sexp = 'a Sexp.sexp
    type 'a slist = 'a Sexp.slist

    local val counter_ref = ref 0 in
        fun cons sexp slist =   let val _ = counter_ref := !counter_ref + 1
                                in Sexp.Cons (sexp, slist) end

        val get_counter = fn _ => !counter_ref
        val set_counter = fn c => counter_ref := c
    end

    end

signature UNDER_COUNTING =
    sig
        type codomain
        val function : int -> codomain
        val get_counter : unit -> int
    end

signature COUNTER_ITERATOR =
    sig
        structure UnderCounting : UNDER_COUNTING
        val super_counter : int -> int
    end

functor CounterIterator (
    structure UnderCounting : UNDER_COUNTING)
    :> COUNTER_ITERATOR
    =
    struct

    structure UnderCounting = UnderCounting
    
    val super_counter = 
        fn n =>
            let fun sc (zero as 0) = UnderCounting.function zero
                |   sc  n = let val _ = UnderCounting.function n
                            in sc (n-1) end

                val _ = sc n
            in UnderCounting.get_counter () end
    end

functor DeepSimple (
    structure Sexp : SEXP
    structure SexpConsCtor : SEXP_CONS_CTOR
    sharing type Sexp.sexp = SexpConsCtor.sexp
    sharing type Sexp.slist = SexpConsCtor.slist)
    :> DEEP where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp in

        fun deep value =
            let fun deep_builder 0 = Atom value
                |   deep_builder n = List (SexpConsCtor.cons (deep_builder (n-1)) Null)
                
                (* Error: explicit type variable cannot be generalized at its binding declaration: 'a
                val memoization_table : (int * 'a Sexp.sexp) list ref = ref [] *)
                val memoization_table = ref [] 

                fun deep_remember n = 
                    let val result = deep_builder n 
                        val _ = memoization_table := (n, result) :: !memoization_table
                        val (ref memo_values) = memoization_table
                    in  {   result = result, memo_table = memo_values } end

            in deep_remember end
    end

    end

functor DeepRemember (
    structure Sexp : SEXP
    structure SexpConsCtor : SEXP_CONS_CTOR
    sharing type Sexp.sexp = SexpConsCtor.sexp
    sharing type Sexp.slist = SexpConsCtor.slist)
    :> DEEP where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local
        open Sexp
    in 

        fun find n ((arg, value) :: pairs_cdr) =
            if arg = n then value else find n pairs_cdr

        fun member n [] = false
        |   member n ((arg, _) :: pairs_cdr) = 
                if arg = n then true else member n pairs_cdr
                     
        fun deep value =
            let val memoization_table = ref []

                fun deep_builder 0 = Atom value
                |   deep_builder n = List (SexpConsCtor.cons (deep_builder (n-1)) Null) 

                fun deep_remember n = 
                    let val result = deep_builder n 
                        val _ = memoization_table := (n, result) :: !memoization_table
                    in result end 

                fun deep_memo n = 
                    if member n (!memoization_table)
                    then let val sexp = find n (!memoization_table)
                            in {result = sexp, memo_table = !memoization_table} end
                    else let val sexp = deep_remember n 
                            in {result = sexp, memo_table = !memoization_table} end
            in deep_memo end


                 
    end

    end

functor DeepMemo (
    structure Sexp : SEXP
    structure SexpConsCtor : SEXP_CONS_CTOR
    sharing type Sexp.sexp = SexpConsCtor.sexp
    sharing type Sexp.slist = SexpConsCtor.slist)
    :> DEEP where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local
        open Sexp
    in 

        fun find n memoization_table =
            let fun A [] = NONE 
                |   A ((arg, value) :: pairs_cdr) = 
                    if arg = n then SOME value else A pairs_cdr
            in A memoization_table end
                     
        fun deep value =
            let val memoization_table = ref []
            
                fun deep_builder 0 = Atom value
                |   deep_builder n =
                        let val {result, ...} = deep_memo (n-1) 
                        in List (SexpConsCtor.cons result Null) end
                and deep_memo n = case find n (!memoization_table)
                                    of  NONE => let val sexp = deep_builder n 
                                                    val _ = memoization_table := (n, sexp) :: !memoization_table
                                                in {result = sexp, memo_table = !memoization_table} end
                                    |   SOME sexp => {result = sexp, memo_table = !memoization_table}
            in deep_memo end
                 
    end


    end
