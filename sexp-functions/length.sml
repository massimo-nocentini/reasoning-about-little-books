
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
                        (* The following is just an attempt to use multiple args, for instance a collector 
                        val L' = ImperativeY.Y_bang_multi_args ImperativeY.Ariety.two_args L
                        *) 
                    in ImperativeY.Y_bang L end
            in length' slist end
    end

    end



structure SexpLengthViaImperativeYFunction = 
    SexpLengthViaImperativeY (structure Sexp = MakeSexp () )

