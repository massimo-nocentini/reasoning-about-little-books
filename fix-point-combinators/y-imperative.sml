
signature ARIETIES_IMPERATIVE = 
    sig
        val zero_args : (unit -> 'a) ref -> unit -> 'a
        val one_arg : ('a -> 'b) ref -> 'a -> 'b
        val two_args : ('a -> 'b -> 'c) ref -> 'a -> 'b -> 'c
    end

functor ArietiesImperative ()
    :> ARIETIES_IMPERATIVE
    =
    struct

    fun zero_args f () = !f ()
    fun one_arg f arg = !f arg
    fun two_args f arg arg1 = !f arg arg1

    end

signature Y_COMBINATOR_IMPERATIVE = 
    sig
        type t
        val Y_bang : (('a -> t) -> 'a -> t) -> 'a -> t
    end

signature Y_COMBINATOR_IMPERATIVE_MULTIARGS = 
    sig
        include Y_COMBINATOR_IMPERATIVE
        structure Ariety : ARIETIES_IMPERATIVE
        val Y_bang_multi_args : (('a -> t) ref -> 'b) -> ('b -> 'a -> t) -> 'a -> t
    end

signature Y_COMBINATOR_IMPERATIVE_DERIVATION = 
    sig
        include Y_COMBINATOR_IMPERATIVE_MULTIARGS
        type 'a sexp
        val length : 'a sexp -> int
        val length' : 'a sexp -> int
        val length'' : 'a sexp -> int
        val length''' : 'a sexp -> int
    end

functor Y_imperative (
    type t
    val initial_value : t
    structure Sexp : SEXP)
(*    :> Y_COMBINATOR_IMPERATIVE_DERIVATION   where type t = t
                                            where type 'a sexp = 'a Sexp.sexp *)
    =
    struct

    type t = t

    structure Ariety = ArietiesImperative()

    open Sexp

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

    fun Y_bang L =
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
    
functor Y_imperative_multiargs (
    type codomain
    val initial_value : codomain
    structure Sexp : SEXP)
    =
    struct
    
    fun Y_bang_multi_args G L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (G h_ref))
            (*val _ = h_ref := (L (G (!h_ref)))*)
        in !h_ref end
     
    end

structure ImperativeY = Y_imperative (
    type t = int 
    val initial_value = 0
    structure Sexp = MakeSexp())

(* output from interactive opening in the repl:
opening ImperativeY
datatype 'a slist = Cons of 'a Sexp.sexp * 'a Sexp.slist | Null
datatype 'a sexp = Atom of 'a | List of 'a Sexp.slist
val length : 'a Sexp.sexp -> int
val length' : 'a Sexp.sexp -> int
val length'' : 'a Sexp.sexp -> int
val length''' : 'a Sexp.sexp -> int
val Y_bang : (('a -> t) -> 'a -> t) -> 'a -> t
val Y_bang_multi_args : (('a -> t) ref -> 'b) -> ('b -> 'a -> t) -> 'a -> t
val G0 : (unit -> 'a) ref -> unit -> 'a
val G1 : ('a -> 'b) ref -> 'a -> 'b
val G2 : ('a -> 'b -> 'c) ref -> 'a -> 'b -> 'c
*)
