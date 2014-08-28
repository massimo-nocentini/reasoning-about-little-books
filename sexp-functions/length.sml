
signature SEXP_LENGTH = 
    sig
        type 'a sexp
        val length : 'a sexp -> int
    end

functor SexpLengthViaImperativeY (
    structure Sexp : SEXP
    structure ImperativeY : Y_COMBINATOR_IMPERATIVE)
    :> SEXP_LENGTH where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp
    in  fun length (List slist) =  
            let val length' =  
                let val L = fn length => 
                                fn  Null => 0 
                                |   Cons (_, cdr_slist) => 1 + length cdr_slist
                    (* The following is just an attempt to use multiple args, for instance a collector 
                    val L' = ImperativeY.Y_bang_multi_args ImperativeY.Ariety.two_args L
                    *) 
                in ImperativeY.Y_bang 0 L end
            in length' slist end
    end

    end

functor SexpLengthViaApplicativeY (
    structure Sexp : SEXP
    structure ApplicativeY : Y_COMBINATOR_APPLICATIVE)
    :> SEXP_LENGTH where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp open ApplicativeY in

        fun length (List slist) =
            let val L = fn recfun =>
                            fn  Null => 0
                            |   (Cons (_, cdr_slist)) => 1 + recfun cdr_slist
            in Y L slist end

    end

    end

functor SexpLengthViaApplicativeYwithAccumulator (
    structure Sexp : SEXP
    structure SelfApplication : SELF_APPLICATION
    structure ApplicativeY : Y_COMBINATOR_APPLICATIVE_MULTIARGS
    sharing type SelfApplication.self = ApplicativeY.self)
    :> SEXP_LENGTH where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp open ApplicativeY open SelfApplication in

        fun length (List slist) =
            let val L = fn recfun =>
                            fn acc => 
                                fn  Null => acc 
                                |   (Cons (_, cdr_slist)) => recfun (1 + acc) cdr_slist
            in Y_multi_args self_apply_two_args L 0 slist end

    end

    end
