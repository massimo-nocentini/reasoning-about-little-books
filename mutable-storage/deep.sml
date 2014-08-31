
signature DEEP = 
    sig
        type 'a sexp
        val deep : 'a -> int -> { result : 'a sexp, memo_table : (int * 'a sexp) list }
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
    structure SexpConsCtor : SEXP_CONS_CTOR sharing type Sexp.sexp = SexpConsCtor.sexp
                                            and type Sexp.slist = SexpConsCtor.slist)
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

signature DEEP_TOPPINGS = 
    sig
        type 'a sexp
        val deep : 'a -> int -> 'a -> 'a sexp
    end

functor DeepToppingsWithLetcc (
    structure Sexp : SEXP
    structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
    :> DEEP_TOPPINGS where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp  in

        fun deep initial_value m = 
            let 
                (*val toppings_ref = ref (fn Atom _ => Atom initial_value)*)
                val toppings_ref = ref (fn atom_sexp as Atom _ => atom_sexp)

                fun deep_rec 0 = HopSkipAndJump.letcc (
                        fn jump => let val _ = toppings_ref := jump in Atom initial_value end)
                |   deep_rec m  = List (Cons (deep_rec (m-1), Null))

                val _ = deep_rec m

(*            in  fn atom => !toppings_ref (Atom atom)  end *)
            in  fn atom => !toppings_ref (Atom atom)  end


    end

    end

functor DeepToppingsWithCont (
    structure Sexp : SEXP
    structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
    :> DEEP_TOPPINGS where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp  in

        fun deep initial_value m = 
            let 
                (*val toppings_ref = ref (fn Atom _ => Atom initial_value)*)
                val toppings_ref = ref NONE

                fun deep_rec 0 = SMLofNJ.Cont.callcc (
                        fn current_cont => 
                            let val _ = toppings_ref := (SOME current_cont) 
                            in Atom initial_value end)
                |   deep_rec m  = List (Cons (deep_rec (m-1), Null))

                val _ = deep_rec m

(*            in  fn atom => !toppings_ref (Atom atom)  end *)
            in  fn atom => let val SOME toppings_cont = !toppings_ref in
                            SMLofNJ.Cont.throw toppings_cont (Atom atom) end end


    end

    end
signature DEEP_WITH_TOPPINGS = 
    sig
        type 'a sexp
        val deep : int -> 'a -> {   result : 'a sexp,
                                    toppings : 'a -> 'a sexp }
    end


functor DeepWithToppingsWithLetcc (
    structure Sexp : SEXP
    structure HopSkipAndJump : HOP_SKIP_AND_JUMP)
    :> DEEP_WITH_TOPPINGS where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    open Sexp

    fun deep m initial_atom =
        let
            fun deep_h 0 = HopSkipAndJump.letcc (
                fn jump => 
                    let fun toppings atom = 
                        let (*val _ = print "hello"  *)
                            val _ = jump {   result = (Atom atom), toppings = toppings } 
                        in Atom atom end
                    in {    result = Atom initial_atom , toppings = toppings } end ) 
            |   deep_h m = 
                let val { result, toppings } = deep_h (m - 1)
                in {result = List (Cons (result, Null)), toppings = toppings} end
        in deep_h m end

    end



