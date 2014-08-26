
signature SEXP_BIZARRE_FOR_Y_IMPERATIVE = 
    sig
        val bizarre: int -> int
    end

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

