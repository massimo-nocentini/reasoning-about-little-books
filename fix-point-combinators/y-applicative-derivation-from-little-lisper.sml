
signature Y_COMBINATOR_APPLICATIVE_DERIVATION_FROM_LITTLE_LISPER =
    sig
        type 'a sexp
        val rember_star : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star'' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star'''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star''''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star'''''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star''''''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star'''''''' : 'a sexp -> ('a -> bool) -> 'a sexp
        val rember_star''''''''' : 'a sexp -> ('a -> bool) -> 'a sexp
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
         The function `curry_maker'  is like the function `R_from_slist' : it takes one
         extra-argument `hukairs' and, when it is applied to an argument (namely, *any* argument),
         it produces a function that looks like `R_from_slist'
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
         our attempt, since ML type system doesn't allow self application.
         So, in order to `curry_maker' consume `curry_maker' (namely, itself!)
         binding it to argument `hukairs', *and* allow `curry_maker' to apply
         `hukairs' to `hukairs' (namely, itself!), we've to set `hukairs' argument
         to has an `Into' form and use the `self_apply' combinator, precisely: 
         in order to `curry_maker' consume itself:
                curry_maker (Into curry_maker)
         and to apply `hukairs' to `hukairs' within `curry_maker':
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

        (* 
         Describe in your own words the function ``curry_maker'':
         When the function ``curry_maker'' is applied to one argument
         `v', such that v has a `Into' constructor shape which
         keeps a function that consumes another function and produces
         ``R_from_slist'', then ``curry_maker'' yields `R_from_slist'.  
         That explanation sounds as if `curry_maker' needs an
         argument that is just like `curry_maker' in order to
         construct `R_from_slist' .
         Do we have to give a name to `curry_maker'? No, because 
         `curry_maker' does not appear within its definition.  
         Do we have to give a name to `R_from_slist'? No, because
         `R_from_slist' does not appear within its definition.  
         True or false: NO recursive function needs to be given a name
         with `fun ...'? True, we chose `R_from_slist' as an arbitrary
         recursive function.
         *)
        fun rember_star''''' (List slist) pred =
            (* 
             Is this definition below the same as the 
             `curry_maker' we defined in the previous step?
             Yes, because for an arbitrary function f such that
             it consumes one value, we can always
             replace f by (fn arg => f arg). 
             *)
            let val curry_maker =
                    fn (hukairs as (Into _)) =>
                        fn  Null => Null
                        |   Cons (atom as Atom a, alist) =>
                            if pred a 
                            then (fn arg => self_apply hukairs arg) alist
                            else Cons (atom, (fn arg => self_apply hukairs arg) alist)
                        |   Cons (List car_slist, cdr_slist) => 
                            Cons (List ((fn arg => self_apply hukairs arg) car_slist), 
                                  (fn arg => self_apply hukairs arg) cdr_slist)
                val R_from_slist = curry_maker (Into curry_maker)
            in List (R_from_slist slist) end

        fun rember_star'''''' (List slist) pred =
            (* 
             Is this definition below the same as the 
             `curry_maker' we defined in the previous step?
             Yes, because the sexp argument does not appear in
             `(fn arg => G future arg)', hence we can abstract out
             this piece, introducing a new name `recfun' with a `fn...'
             and binding it to `(fn arg => G future arg)'. *)
            let val curry_maker =
                    fn (hukairs as (Into _)) =>
                        (fn recfun =>
                            fn  Null => Null
                            |   Cons (atom as Atom a, alist) =>
                                if pred a then recfun alist else Cons (atom, recfun alist)
                            |   Cons (List car_slist, cdr_slist) => 
                                Cons (List (recfun car_slist), recfun cdr_slist)) (fn arg => self_apply hukairs arg)
                val R_from_slist = curry_maker (Into curry_maker)
            in List (R_from_slist slist) end

        (* 
         Can you make the definition of ``curry_maker'' simpler by
         breaking it up into two functions? Yes, because it is safe to 
         name the expression `(fn recfun => ...)', since all the variables
         are explicit arguments or constructor opened from structures.
         *)
        fun rember_star''''''' (List slist) pred =
            let val R = fn recfun =>
                            fn  Null => Null
                            |   Cons (atom as Atom a, alist) =>
                                if pred a then recfun alist else Cons (atom, recfun alist)
                            |   Cons (List car_slist, cdr_slist) => 
                                Cons (List (recfun car_slist), recfun cdr_slist)
                val curry_maker = fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)
                val R_from_slist = curry_maker (Into curry_maker)
            in List (R_from_slist slist) end

        (* 
         Write `R_from_slist' without using `curry_maker'! Hint:
         use the most recent definition of `curry_maker' in two
         different places or, in other words, inline it in the 
         definition of `R_from_slist'. After that, remove `curry_maker'!
         *)
        fun rember_star'''''''' (List slist) pred =
            let val R = fn recfun =>
                            fn  Null => Null
                            |   Cons (atom as Atom a, alist) =>
                                if pred a then recfun alist else Cons (atom, recfun alist)
                            |   Cons (List car_slist, cdr_slist) => 
                                Cons (List (recfun car_slist), recfun cdr_slist)
                val R_from_slist = (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)) 
                                        (Into (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)))
            in List (R_from_slist slist) end


        (* 
         Abstract the definition of `R_from_slist' by abstracting away
         the association with `R' creating a new value, call it Y!
         Hint: wrap a (fn R => ...) around the definition...
         *)
        fun rember_star''''''''' (List slist) pred =
            let val R = fn recfun =>
                            fn  Null => Null
                            |   Cons (atom as Atom a, alist) =>
                                if pred a then recfun alist else Cons (atom, recfun alist)
                            |   Cons (List car_slist, cdr_slist) => 
                                Cons (List (recfun car_slist), recfun cdr_slist)
                val Y =  fn R => 
                            (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)) 
                                (Into (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)))
                val R_from_slist = Y R
            in List (R_from_slist slist) end

        (* 
         You have just worked through the derivation of a function
         called ``the applicative-order Y-combinator''. The interesting
         aspect of Y is that it produces recursive definitions without the
         bother of requiring that the functions be named with ``fun ...''
         *)
        val Y =  fn R => 
                    (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)) 
                        (Into (fn (hukairs as (Into _)) => R (fn arg => self_apply hukairs arg)))
    end
    
    
    end 
