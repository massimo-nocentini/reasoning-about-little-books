
signature COUNTER = 
    sig
        val get_counter: unit -> int
        val set_counter: int -> unit
    end


signature UNDER_COUNTING =
    sig
        (* 
         Again, fixing a type here force ourself to do the job of 
         the type system, in particular we've to infer and hard code
         the type in every structure that abscribes to this signature.
         For example:
            structure UnderCounting = 
                struct
                    type codomain = { result : pizza SexpStr.sexp, memo_table : (int * pizza SexpStr.sexp) list}
                    val function = deep_simple
                    val get_counter = SexpConsCtorCountingForDeepSimple.get_counter
                end
        As we can see, polymorphism is reduced, since we've to write `pizza SexpStr.sexp', that
        is instantiate type variable `a' of `'a sexp' with type pizza. So, this is 
        a working solution but not elegant. 
         *)
        (* type codomain *)

        (*
         If we try to generalize the previous observation about `type codomain',
         introducing a type variable, yielding `type 'a codomain', we run against
         two issues:
            1. We do not know if a single type variable is enough for any possible
                use of the type. In our case of `'a sexp' it is, but this is only
                one possibile type instantiation;
            2. It doesn't type check, here the error:
                Error: value type in structure doesn't match signature spec
                name: function
                spec:   int -> 'a ?.UnderCounting.codomain
                actual: int -> {memo_table:(int * pizza sexp) list, result:pizza sexp}

                produced with the following use:
                type 'a codomain = { result : 'a SexpStr.sexp, memo_table : (int * 'a SexpStr.sexp) list}
                Maybe we can fix it abscribing the working structure to a signature refined with `where type'
                but this cause adding boilerplate code.
         *)
        (* type 'a codomain *)

        (*
         This is another attempt, we request a function that consumes an integer
         and produces a value of any type. Hence if a functor asks for a 
         structure with this shape, we've to supply a structure with a function
         that really consumes an integer and, in particular, produces a value of 
         any value. This request cannot be satisfied at all (except some cases, 
         see for example SMLofNJ.Cont.throw) since any function that we want
         to use under counting, returns a value of a specified type, not a 
         general one. Hence this approach doesn't type check, as the output confirm:
         Error: value type in structure doesn't match signature spec
             name: function
             spec:   int -> 'a
             actual: int -> {memo_table:(int * pizza sexp) list, result:pizza sexp}
         *)
        (* val function : int -> 'a *)

        (*
         The following is our best effort to have an elegant solution.
         This signature only requires a type, in particular an arrow type,
         that will be used in COUNTER_ITERATOR.super_counter to request
         a function to be used under counting. This allow to supply
         a function regardless of the value's type it produces.
         This is the major difference between the previous approach 
            (val function : int -> 'a) where is required to supply a function 
            that produces a value of any type;
            this approach (type 'a fuc = int -> 'a and
                val super_counter : 'a UnderCounting.fuc -> int -> int)
            only requires that a function that consumes an int and produces
            whatever value it likes to be fed to `super_counter'.
         *)
        (* `fuc' stands for "Function Under Counting" *)
        type 'a fuc = int -> 'a  
        val get_counter : unit -> int
    end
