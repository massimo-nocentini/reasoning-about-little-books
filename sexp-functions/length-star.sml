
functor SexpLengthStarViaApplicativeYwithCollector (
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
                            fn col => 
                                fn  Null => col 0 
                                |   (Cons (Atom _, cdr_slist)) => 
                                        recfun (fn cdr_length => col (1 + cdr_length)) cdr_slist
                                |   (Cons (List car_slist, cdr_slist)) =>
                                        recfun (fn car_length => 
                                                    recfun (fn cdr_length => col (car_length + cdr_length)) 
                                                            cdr_slist) 
                                                car_slist
            in Y_multi_args self_apply_two_args L (fn length => length) slist end

    end

    end
