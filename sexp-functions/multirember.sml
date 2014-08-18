
signature SEXP_MULTIREMBER = 
	sig
		type 'a sexp

		val multirember : 'a sexp -> 'a sexp -> ('a -> 'a -> bool) -> 'a sexp
	end

	(*
	 TODO: after the Y combinator finds its home in a signature,
	 write a functor that uses it to define a structure that implements
	 SEXP_MULTIREMBER.
	*)

functor SexpMultirember (
	structure Sexp : SEXP
	structure SexpEqualFunction : SEXP_EQUAL
	sharing type Sexp.sexp = SexpEqualFunction.sexp)
	:> SEXP_MULTIREMBER where type 'a sexp = 'a Sexp.sexp
	=
	struct
		
		open Sexp

		fun multirember aSexp anElement equality_comparer = 
			let 
				(* The definition of `multirember_sexp' refers to
				`anElement' which stands for the atom that we need to
				remove from `aSexp'. A function defined in this `naming
				part' (corresponding to (letrec ...) naming part) knows
				all the arguments of all the surrounding `fun's
				expressions.*)
				fun multirember_sexp (List conses) = List (multirember_slist conses)
				and multirember_slist Null = Null
				|	multirember_slist (Cons (car_sexp, cdr_slist)) = 
					if SexpEqualFunction.equal equality_comparer car_sexp anElement 
					then multirember_slist cdr_slist
					else Cons (car_sexp, multirember_slist cdr_slist)
			in multirember_sexp aSexp end

	(*************************************************************
	 The following stuff is here just to implement the steps taken by book.
	 The previous version is the final one we should use, since the following ones
	 just use some tricks that naively we already implement them (for example the equality_comparer
		 function resembles almost the following code. Hence we do not create different functors 
		 to show refactorings step by step).
	 *************************************************************)

		(* Can you define the function `multirember_with_test' which
		relates to `multirember' consuming a function to abstract test
		logic? The function `multirember_with_test' accepts a function
		`test' and returns a new function, let us call this latter
		function `m_f'. The function `m_f' takes a sexp `anElement' and a
		list of sexps and traverses the latter. Any sexp `b' in the list
		for which `test anElement b' is true, is removed. Is it true that
		during this traversal the result of `multirember_with_test aTest'
		is always the same? Yes, it is always the function for which we
		just used the name `m_f'... *)
		fun multirember_with_test test anElement (List conses) = 
			multirember_with_test_slist test anElement conses
		and multirember_with_test_slist test anElement Null = Null
		|	multirember_with_test_slist test anElement (Cons (car_sexp, cdr_slist)) = 
			if test anElement car_sexp 
			then multirember_with_test_slist test anElement cdr_slist
			else Cons (car_sexp, multirember_with_test_slist test anElement cdr_slist)

		(* ...Perhaps `multirember_with_test should name it `m_f' *)
		fun multirember_with_test_slist_abridged_1 test = 
			let
				fun m_f anElement (List conses) = m_f_slist anElement conses
				and m_f_slist anElement Null = Null
				|	m_f_slist anElement (Cons (car_sexp, cdr_slist)) = 
					if test anElement car_sexp 
					then m_f_slist anElement cdr_slist
					else Cons (car_sexp, m_f_slist anElement cdr_slist)
			in m_f end

		(* What is the value of `multirember_with_test_slist_abridged_1
		aTest' where `aTest' is the equal comparison among sexps? *)
		val multirember_with_test_slist_abridged_2 = 
			let
				fun curried_equal a b = a = b

				fun m_r anElement (List conses) = m_r_slist anElement conses
				and m_r_slist anElement Null = Null
				|	m_r_slist anElement (Cons (car_sexp, cdr_slist)) = 
					if SexpEqualFunction.equal curried_equal anElement car_sexp 
					then m_r_slist anElement cdr_slist
					else Cons (car_sexp, m_r_slist anElement cdr_slist)

			in m_r end

		(* Did you notice that no `fun ...' surrounds the
		`let...in...end'? It looks odd, but is correct! Could we have used
		another name for the function named in `let...in...end'? Yes,
		`m_r' is `multirember_with_test_slist_abridged_2' *)
		val multirember_with_test_slist_abridged_2 = 
			let
				fun curried_equal a b = a = b

				fun multirember_with_test_slist_abridged_2 anElement (List conses) = 
					multirember_with_test_slist_abridged_2_slist anElement conses
				and multirember_with_test_slist_abridged_2_slist anElement Null = Null
				|	multirember_with_test_slist_abridged_2_slist anElement (Cons (car_sexp, cdr_slist)) = 
					if SexpEqualFunction.equal curried_equal anElement car_sexp 
					then multirember_with_test_slist_abridged_2_slist anElement cdr_slist
					else Cons (car_sexp, multirember_with_test_slist_abridged_2_slist anElement cdr_slist)
			in multirember_with_test_slist_abridged_2 end

		(* Since `let...in...end' defines a recursive function and since
		`val ... = ...' pairs up names with values, we could eliminate
		`let...in...end' here, right? Yes, we could and we would get back
		our old friend `multirember'. *)
		fun multirember_with_test_slist_abridged_3 anElement aSexp = 
			let
				fun curried_equal a b = a = b

				fun multirember_sexp (List conses) = multirember_slist conses
				and multirember_slist Null = Null
				|	multirember_slist (Cons (car_sexp, cdr_slist)) = 
					if SexpEqualFunction.equal curried_equal anElement car_sexp 
					then multirember_slist cdr_slist
					else Cons (car_sexp, multirember_slist cdr_slist)
			in multirember_sexp aSexp end


	end
