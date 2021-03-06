
signature SEXP_BIZARRE_FOR_Y_IMPERATIVE = 
    sig
        val bizarre: int -> int
    end

(*
 The following two functions look a bit ugly since the duplication
 of function `B', we should factor it but the interesting thing
 to get is written.
 *)

functor SexpBizarreForImperativeY (
    structure ImperativeY : Y_COMBINATOR_IMPERATIVE)
    :> SEXP_BIZARRE_FOR_Y_IMPERATIVE 
    =
    struct

    val bizarre =   let val x = ref 0
                        val B = fn f => let val _ = x := !x + 1 
                                            in (fn a => if a = !x then 0 else f a) end
                    in  ImperativeY.Y_bang 0 B  end 

    end

functor SexpBizarreForApplicativeY (
    structure ApplicativeY : Y_COMBINATOR_APPLICATIVE)
    :> SEXP_BIZARRE_FOR_Y_IMPERATIVE 
    =
    struct

    val bizarre =   let val x = ref 0
                        val B = fn f => let val _ = x := !x + 1 
                                            in (fn a => if a = !x then 0 else f a) end
                    in  ApplicativeY.Y B  end 

    end

