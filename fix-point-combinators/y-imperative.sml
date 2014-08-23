
signature Y = 
    sig
    (* To be filled at the end of the implementation :) *)

    end


functor Y_imperative (
    type t
    val initial_value : t
    structure Sexp : SEXP)
    =
    struct

    open Sexp

    fun length (List slist) =
        let fun L Null = 0
            |   L (Cons (_, cdr_slist)) = 1 + L cdr_slist
        in L slist end

    val length' = 
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

        
    val length'' = 
        let val h_ref = ref (fn _ => 0) 
            val _ = h_ref := ((fn length => 
                                fn  Null => 0
                                |   Cons (_, cdr_slist) => 1 + length cdr_slist) (fn arg => !h_ref arg))
                                (*|   Cons (_, cdr_slist) => 1 + length cdr_slist) (!h_ref))*)
        in !h_ref end
             
    val length''' = 
        let val h_ref = ref (fn _ => 0) 
            val L = fn length =>   
                        fn  Null => 0
                        |   Cons (_, cdr_slist) => 1 + length cdr_slist
            (*val _ = h_ref := (L (!h_ref)) *)
            val _ = h_ref := (L (fn arg => !h_ref arg))
        in !h_ref end

    fun Y! L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (fn arg => !h_ref arg))
        in !h_ref end

    fun Y_multi_args! G L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (G h_ref))
            (*val _ = h_ref := (L (G (!h_ref)))*)
        in !h_ref end

    fun G1 f arg = !f arg

    (* 
     The following should be an attempt to translate Y-bang, page 123 ,
     but it doesn't work since `val rec' require a function declaration
     on its right hand side.
     *)
    (*fun Y_bang f = let val rec h = (fn f => f (fn arg => h arg))  in h f end*)
    end
    
    (*
     TODO: move the following signature and functor in `sexp-functions' folder
     and write some unit tests for `length' function. Moreover, for each
     functor that we've written we should remove signature abscription
     since in this way we constrain how a functor can be used, limiting
     the many-to-many capability of signature abscription (ie, only
     functors' client should abscribe their structure built using functors
     to a signature of interest.
     *)

    signature SEXP_LENGTH = 
        sig
            type 'a sexp
            val length : 'a sexp -> int
        end

    functor SexpLengthViaImperativeY (
        structure Sexp : SEXP)
        :> SEXP_LENGTH where type 'a sexp = 'a Sexp.sexp
        =
        struct

        type 'a sexp = 'a Sexp.sexp

        structure ImperativeY = Y_imperative (
            type t = int 
            val initial_value = 0
            structure Sexp = Sexp)

        local open Sexp
        in  fun length (List slist) =  
                let val length' =  
                        let val L = fn length =>   
                                        fn  Null => 0 
                                        |   Cons (_, cdr_slist) => 1 + length cdr_slist
                            val L' = ImperativeY.Y_multi_args! ImperativeY.G1 L
                        in ImperativeY.Y! L end
                in length' slist end
        end

        end
