
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

        (* Do you remember `length'?  *)
        fun length (List slist) =
            let fun L Null = 0
                |   L (Cons (_, cdr_slist)) = 1 + L cdr_slist
            in L slist end

        fun length' (List slist) = 
            (*
             Does this one disregard the The Seventeenth Commandment? There is no (fn...)
             between `let val h_ref...' and `href := ...'.
             Final version of the Commandment: Use `x := ' for 'let val x = ref ...' only
             if there is at least one `(fn...)' or `fun ...' between it and the `let', or
             if the new value for `x' is a function that refers to `x'.
             *)
            let val L' = 
                    (*
                     Here is the abridged version of `length' which use mutable storage. Can you 
                     eliminate the parts of the definition that are specific to `length'?
                     The right-hand side of `h_ref := ...' needs to be eliminated, moreover
                     we should remember to remove the initial value, `0', in `(fn => 0)' too.
                     The rest could be reused to construct any recursive function of one argument.
                     *)
                    let val h_ref = ref (fn _ => 0) 
                        val _ = h_ref := (  fn  Null => 0
                                            |   Cons (_, cdr_slist) => 1 + !h_ref cdr_slist )
                    in !h_ref end
                    (* 
                     The following commented line of code does produce the
                     following type checking error that I do not understand:
                        let val h_ref : ('a Sexp.slist -> int) ref = ref (fn _ => 0) 
                     >> Error: explicit type variable cannot be generalized at its binding declaration: 'a 
                     With this attempt we would like to fix the type of `h_ref' 
                     just for ease the derivation.
                     *)
            in L' slist end
            
        fun length'' (List slist) = 
            let val L'' =
                let val h_ref = ref (fn _ => 0) 
                    (*
                     Good step. We've abstracted out `!h_ref' introducing a name `length' for it, 
                     in particular we use `(fn arg => !h_ref arg) ' as value instead of `!h_ref',
                     because the former represent a "delayed" computation where each time it is
                     invoked we use the reference operator `!' hence, since `h_ref' represent
                     something with a mutable nature, we always get the most recent referenced function 
                     assigned to `h_ref'. On the other hand, the latter value `!h_ref' for `length'
                     argument strictly evaluate the referenced function, in this case `(fn _ => 0) '
                     defined in the previous line of code, producing a function that embeds it
                     for all the following use, having a not working definition, defined but not working.

                     *)
                    val _ = h_ref := ((fn length => 
                                        fn  Null => 0
                                        |   Cons (_, cdr_slist) => 1 + length cdr_slist) (fn arg => !h_ref arg))
                                        (*|   Cons (_, cdr_slist) => 1 + length cdr_slist) (!h_ref))*)
                in !h_ref end
            in L'' slist end
                 
        fun length''' (List slist) = 
            let val L''' =
                let val h_ref = ref (fn _ => 0) 
                    
                    (*
                     This step extract the previous abstracted chunk of code, binding its value
                     to `L' and use it in the update of `h_ref'. 
                     *)
                    val L = fn length =>   
                                fn  Null => 0
                                |   Cons (_, cdr_slist) => 1 + length cdr_slist

                    (*val _ = h_ref := (L (!h_ref)) ***** WRONG ***** -> early deferencing of `h_ref' *)
                    val _ = h_ref := (L (fn arg => !h_ref arg))
                in !h_ref end
            in L''' slist end

    end (* ending: `local open Sexp in' *)
    
    (* 
     Rewrite the definition of `L'' ' so that it becomes a function of `L' and
     remember to abstract the initial dummy value too. Call the new function `Y_bang'.
     *)
    fun Y_bang initial_value L =
        let val h_ref = ref (fn _ =>  initial_value) 
            val _ = h_ref := (L (fn arg => !h_ref arg))
        in !h_ref end

    (* 
     The following should be an attempt to translate Y-bang, page 123 ,
     but it doesn't work since `val rec' require a function declaration
     on its right hand side. Maybe a possible solution is to use lazy
     suspension changing compiler directives in sources.cm
     *)
    (*fun Y_bang f = let val rec h = (fn f => f (fn arg => h arg))  in h f end*)

    (*************************************************************
     You have just worked through the derivation of a function called
     "the applicative-order, imperative Y combinator." The interesting
     aspect of `Y_bang' is that it produces recursive definitions without
     requiring that functions be named by `fun...'. 
     How do we go from a recursive function definition to a function `fun f...'
     such that `Y_bang f' builds the corresponding recursive function
     without `fun ...'? Felleisen and Friedman words: " `f' is like the
     recursive function except that the name of the recursive function is
     replaced by the name `recfun' and the whole expression is wrapped in
     `(fn recfun => ...)'. 
     Is it true that the value of `Y f' (Y is the "standard" applicative Y 
     combinator) is the same recursive function as the value of `Y_bang f'?
     Yes, the combinator `Y_bang' produces the same recursive functions as `Y'
     for all function `f' that has this shape (ie. the shape described in the 
     previous paragraph). What happens when we use `Y' and `Y_bang' with
     a function that does not have this shape? Look at definition in 
     SexpBizarreForImperativeY functor and the corresponding test cases :) 
    *************************************************************)

    end
