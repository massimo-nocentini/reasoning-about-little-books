
signature DEEP = 
    sig
        type t
        type 'a sexp

        val deep : int -> { result : t sexp,
                            memo_table : (int * t sexp) list }
    end

functor DeepSimple (
    type t
    val value : t
    structure Sexp : SEXP)
    :> DEEP where type t = t 
            where type 'a sexp = t Sexp.sexp
    =
    struct

    type t = t
    type 'a sexp = t Sexp.sexp

    (* the following is just an alias in order to not hide 'a sexp by the opening of structure Sexp *)
    type typed_sexp = t Sexp.sexp

    local open Sexp in

        fun deep_builder 0 = Atom value
        |   deep_builder n = List (Cons (deep_builder (n-1), Null))

        local
            val memoization_table : (int * typed_sexp) list ref = ref []
        in
            fun deep_remember n = 
                let val result = deep_builder n 
                    val _ = memoization_table := (n, result) :: !memoization_table
                    val (ref memo_values) = memoization_table
                in  {   result = result, memo_table = memo_values } end


        end

    val deep = deep_remember
        
    end

    end

functor DeepRemember (
    type t
    val value : t
    structure Sexp : SEXP)
    :> DEEP where type t = t 
            where type 'a sexp = t Sexp.sexp
    =
    struct

    type t = t
    type 'a sexp = t Sexp.sexp

    (* the following is just an alias in order to not hide 'a sexp by the opening of structure Sexp *)
    type typed_sexp = t Sexp.sexp

    local
        open Sexp
        val memoization_table : (int * typed_sexp) list ref = ref []
    in 

        fun find (n : int) =
            let fun A ((arg, value) :: pairs_cdr) = 
                    if arg = n then value else A pairs_cdr
            in A (!memoization_table) end

        fun member (n : int) =
            let fun M [] = false
                |   M ((arg, _) :: pairs_cdr) = 
                        if arg = n then true else M pairs_cdr
            in M (!memoization_table) end
        
                     
        fun deep_builder 0 = Atom value
        |   deep_builder n = List (Cons (deep_builder (n-1), Null)) 

        fun deep_remember n = 
            let val result = deep_builder n 
                val _ = memoization_table := (n, result) :: !memoization_table
            in result end 

        fun deep_memo n = 
            if member n
            then let val sexp = find n 
                    in {result = sexp, memo_table = !memoization_table} end
            else let val sexp = deep_remember n 
                    in {result = sexp, memo_table = !memoization_table} end


                 
    end

    val deep = deep_memo

    end

functor DeepMemo (
    type t
    val value : t
    structure Sexp : SEXP)
    :> DEEP where type t = t 
            where type 'a sexp = t Sexp.sexp
    =
    struct

    type t = t
    type 'a sexp = t Sexp.sexp

    (* the following is just an alias in order to not hide 'a sexp by the opening of structure Sexp *)
    type typed_sexp = t Sexp.sexp

    local
        open Sexp
        val memoization_table : (int * typed_sexp) list ref = ref []
    in 

        fun find (n : int) =
            let fun A ((arg, value) :: pairs_cdr) = 
                    if arg = n then value else A pairs_cdr
            in A (!memoization_table) end

        fun member (n : int) =
            let fun M [] = false
                |   M ((arg, _) :: pairs_cdr) = 
                        if arg = n then true else M pairs_cdr
            in M (!memoization_table) end
        

                     
        fun deep_builder 0 = Atom value
        |   deep_builder n =
                let val {result, ...} = deep_memo (n-1) 
                in List (Cons (result, Null)) end
        and deep_remember n = 
            let val result = deep_builder n 
                val _ = memoization_table := (n, result) :: !memoization_table
            in result end 
        and deep_memo n = 
            if member n
            then let val sexp = find n 
                    in {result = sexp, memo_table = !memoization_table} end
            else let val sexp = deep_remember n 
                    in {result = sexp, memo_table = !memoization_table} end


                 
    end

    val deep = deep_memo

    end
