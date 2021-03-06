
(* 
 In the following we would like to define a signature
 that allow us to count how many times the Cons 
 constructor will be applied. In order to do this
 just put together the signature that define the 
 cons operation mapping to Cons constructor and
 pair it with the idea of a generic counter, very
 orthogonal an simple to understand.
*)
signature SEXP_CONS_CTOR_COUNTING = 
    sig
        include SEXP_CONS_CTOR
        include COUNTER
    end

functor SexpConsCtorCounting (
    structure Sexp : SEXP)
    :> SEXP_CONS_CTOR_COUNTING  where type 'a sexp = 'a Sexp.sexp
                                where type 'a slist = 'a Sexp.slist
    =
    struct

    type 'a sexp = 'a Sexp.sexp
    type 'a slist = 'a Sexp.slist

    local val counter_ref = ref 0 in
        fun cons sexp slist =   let val _ = counter_ref := !counter_ref + 1
                                in Sexp.Cons (sexp, slist) end

        val get_counter = fn _ => !counter_ref
        val set_counter = fn c => counter_ref := c
    end

    end
