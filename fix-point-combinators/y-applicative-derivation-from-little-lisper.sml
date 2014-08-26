
(* Give me time, we'll write our signatures very soon :) *)
signature Y_COMBINATOR_APPLICATIVE_DERIVATION_FROM_LITTLE_LISPER =
    sig
        type 'a sexp
        val rember_star : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star'' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star''' : 'a sexp -> ('a -> bool) -> 'a sexp
    end

functor Y_applicative_derivation_from_little_lisper (
    structure Sexp : SEXP)
    :> Y_COMBINATOR_APPLICATIVE_DERIVATION_FROM_LITTLE_LISPER where type 'a sexp = 'a Sexp.sexp
    =
    struct

    type 'a sexp = 'a Sexp.sexp

    local open Sexp in

        (* Here it is the verbose version, using the Commandments *)
        fun rember_star sexp pred = 
            let fun R_from_slist Null = Null
                |   R_from_slist (Cons (sexp, slist)) =
                    let fun first_sexp_is_atom_to_be_removed (Atom a) =  pred a
                          | first_sexp_is_atom_to_be_removed (List _) = false
                    in  if first_sexp_is_atom_to_be_removed sexp 
                        then R_from_slist slist
                        else Cons ((R_from_sexp sexp), (R_from_slist slist))
                    end
                and R_from_sexp (original as Atom a) = original
                |   R_from_sexp (List slist) = List (R_from_slist slist)
            in R_from_sexp sexp end

        (* Here is an abridged version that use pattern matching *)
        fun rember_star' (List slist) pred = 
            let fun R_from_slist Null = Null
                |   R_from_slist (Cons (atom as Atom a, alist)) = 
                    if pred a then R_from_slist alist else Cons (atom, R_from_slist alist)
                |   R_from_slist (Cons (List fsList, snList)) = 
                    Cons ((List (R_from_slist fsList)), (R_from_slist snList))
            in List (R_from_slist slist) end

        (* 
         The function `rember_star''  is like the function `rember_star' : it takes one
         extra-argument `hukairs' and, when it is applied to an argument (namely, *any* argument),
         it produces a function that looks like `rember_star'
         except for the application ``curry_maker hukairs alist''. 
         *)
        fun rember_star'' (List slist) pred = 
            let val rec curry_maker = 
                    fn hukairs =>
                        fn  Null => Null
                        |   Cons (atom as Atom a, alist) =>
                            if pred a 
                            then curry_maker hukairs alist
                            else Cons (atom, curry_maker hukairs alist)
                        |   Cons (List car_slist, cdr_slist) => 
                            Cons (List (curry_maker hukairs car_slist), (curry_maker hukairs cdr_slist))
                (* 
                 The argument for the parameter `hukairs' is just
                 passed around: when `curry_maker' reaches the leafs of the sexp,
                 `hukairs' is not used, hence we can bind `hukairs' with any value we want.
                 Can `curry_maker' define `R_from_slist'? Yes, and doesn't matter what
                 we use to define `R_from_slist', since `hukairs' is never used.
                 *)
                val R_from_slist = curry_maker 0
            in List (R_from_slist slist) end


        fun rember_star''' (List slist) pred = 
            let val rec curry_maker =
                    fn hukairs =>
                        fn  Null => Null
                        |   Cons (atom as Atom a, alist) =>
                            if pred a 
                            then curry_maker hukairs alist
                            else Cons (atom, curry_maker hukairs alist)
                        |   Cons (List car_slist, cdr_slist) => 
                            Cons (List (curry_maker hukairs car_slist), (curry_maker hukairs cdr_slist))
                (* 
                 Can we use ``curry_maker'' to define `R_from_slist' binding argument
                 `hukairs' with `curry_maker'? Of course, thus `hukairs' become `curry_maker' itself!
                 *)
                val R_from_slist = curry_maker curry_maker
            in List (R_from_slist slist) end

        datatype 'a T = Into of 'a T -> 'a

        (* fun Y f = H f (Into (H f)) *)
        (* (* and H f into = f (G into) *) *)
        (* and H f = f o G *)
        fun self_apply (into as Into aFn) x = aFn into x
        (* using the following definition the type-checker takes very long
         time to type-check the phrase.*)
        (* and G (into as Into aFn) = aFn into *)

        (* 
         Take a look at this application, which occurs three times in the
         previous definition of `curry_maker': curry_maker hukairs 
         Since `hukairs' is bound to `curry_maker' can we use `hukairs'
         where we use `curry_maker'? Of course, but not directly. We've
         to resort to type `'a T' and `self_apply' combinator to type check 
         our attempt since ML type system doesn't allow self application.
         So, in order to `curry_maker' consume `curry_maker' (namely, itself!)
         binding it to argument `hukairs', *and* allow `curry_maker' to apply
         `hukairs' to `hukairs' (namely, itself!), we've to use the `Into'
         type constructor and `self_apply' combinator. 
         Hence, in order to `curry_maker' consume itself:
                curry_maker (Into curry_maker)
         And to apply `hukairs' to `hukairs' within `curry_maker':
                self_apply hukairs
         Why does this definition will work? Because the value of 
         `self_apply hukairs' is the same as `curry_maker (Into curry_maker)' 
         which is the same as `R_from_slist'
         *)
        fun rember_star'''' (List slist) pred =
            (* Pay attention that `curry_maker' definition is not `rec'-ursive anymore! *) 
            let val curry_maker =
                    fn (hukairs as (Into _)) =>
                        fn  Null => Null
                        |   Cons (atom as Atom a, alist) =>
                            if pred a   
                            then self_apply hukairs alist 
                            else Cons (atom, self_apply hukairs alist)
                        |   Cons (List car_slist, cdr_slist) => 
                            Cons (List (self_apply hukairs car_slist), 
                                        self_apply hukairs cdr_slist)
                val R_from_slist = curry_maker (Into curry_maker)
            in List (R_from_slist slist) end

    end
    
    
    end 
