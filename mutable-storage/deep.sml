
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
        (* 
         Again, fixing a type here force ourself to do the job of 
         the type system, in particular we've to infer and hard code
         the type in every structure that abscribes to this signature.
         For example:
            structure UnderCounting = 
                struct
                    type codomain = { result : pizza SexpStr.sexp, memo_table : (int * pizza SexpStr.sexp) list}
                    val function = deep_simple
                    val get_counter = SexpConsCtorCountingForDeepSimple.get_counter
                end
        As we can see, polymorphism is reduced, since we've to write `pizza SexpStr.sexp', that
        is instantiate type variable `a' of `'a sexp' with type pizza. So, this is 
        a working solution but not elegant. 
         *)
        (* type codomain *)

        (*
         If we try to generalize the previous observation about `type codomain',
         introducing a type variable, yielding `type 'a codomain', we run against
         two issues:
            1. We do not know if a single type variable is enough for any possible
                use of the type. In our case of `'a sexp' it is, but this is only
                one possibile type instantiation;
            2. It doesn't type check, here the error:
                Error: value type in structure doesn't match signature spec
                name: function
                spec:   int -> 'a ?.UnderCounting.codomain
                actual: int -> {memo_table:(int * pizza sexp) list, result:pizza sexp}

                produced with the following use:
                type 'a codomain = { result : 'a SexpStr.sexp, memo_table : (int * 'a SexpStr.sexp) list}
                Maybe we can fix it abscribing the working structure to a signature refined with `where type'
                but this cause adding boilerplate code.
         *)
        (* type 'a codomain *)

        (*
         This is another attempt, we request a function that consumes an integer
         and produces a value of any type. Hence if a functor asks for a 
         structure with this shape, we've to supply a structure with a function
         that really consumes an integer and, in particular, produces a value of 
         any value. This request cannot be satisfied at all (except some cases, 
         see for example SMLofNJ.Cont.throw) since any function that we want
         to use under counting, returns a value of a specified type, not a 
         general one. Hence this approach doesn't type check, as the output confirm:
         Error: value type in structure doesn't match signature spec
             name: function
             spec:   int -> 'a
             actual: int -> {memo_table:(int * pizza sexp) list, result:pizza sexp}
         *)
        (* val function : int -> 'a *)

        (*
         The following is our best effort to have an elegant solution.
         This signature only requires a type, in particular an arrow type,
         that will be used in COUNTER_ITERATOR.super_counter to request
         a function to be used under counting. This allow to supply
         a function regardless of the value's type it produces.
         This is the major difference between the previous approach 
            (val function : int -> 'a) where is required to supply a function 
            that produces a value of any type;
            this approach (type 'a fuc = int -> 'a and
                val super_counter : 'a UnderCounting.fuc -> int -> int)
            only requires that a function that consumes an int and produces
            whatever value it likes to be fed to `super_counter'.
         *)
        (* `fuc' stands for "Function Under Counting" *)
        type 'a fuc = int -> 'a  
        val get_counter : unit -> int
    end

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
