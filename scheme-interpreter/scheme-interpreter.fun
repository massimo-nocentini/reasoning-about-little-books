
functor SchemeInterpreterEnvironment(structure Sexp: SEXP) = 
    struct

    structure Table = MakeTableDoubleListImpl (
	structure IdentifierType = MakeTypeString ())

    structure SexpFunctions = SexpFunctionsStandardImpl(
	structure Sexp = Sexp)

    datatype scheme_term = TmInteger of int
			 | TmBoolean of bool
			 | TmCons
			 | TmCar 
			 | TmCdr
			 | TmNull_p
			 | TmEq_p
			 | TmAtom_p			     
			 | TmZero_p
			 | TmSucc
			 | TmPred
			 | TmNumber_p
			 | TmIdentifier of Table.identifier
			 | TmQuote
			 | TmLambda
			 | TmCond
			 | TmElse

    datatype scheme_meaning = Primitive of scheme_term Sexp.sexp
			    | Quotation of scheme_term Sexp.sexp
			    | NonPrimitive of closure withtype closure = {
				table: scheme_meaning Table.table,
				formals: scheme_term Sexp.sexp,
				body: scheme_term Sexp.sexp
			    }

    fun scheme_term_equal (TmInteger fst_int) (TmInteger snd_int) = 
	fst_int = snd_int
      | scheme_term_equal (TmBoolean fst_bool) (TmBoolean snd_bool) =
	fst_bool = snd_bool
      | scheme_term_equal TmCons TmCons = true
      | scheme_term_equal TmCar TmCar = true
      | scheme_term_equal TmCdr TmCdr = true
      | scheme_term_equal TmNull_p TmNull_p = true
      | scheme_term_equal TmEq_p TmEq_p = true
      | scheme_term_equal TmAtom_p TmAtom_p = true
      | scheme_term_equal TmZero_p TmZero_p = true
      | scheme_term_equal TmSucc TmSucc = true
      | scheme_term_equal TmPred TmPred = true
      | scheme_term_equal TmNumber_p TmNumber_p = true
      | scheme_term_equal (TmIdentifier fst_id) (TmIdentifier snd_id) = 
	fst_id = snd_id
      | scheme_term_equal TmQuote TmQuote = true
      | scheme_term_equal TmLambda TmLambda = true
      | scheme_term_equal TmCond TmCond = true
      | scheme_term_equal TmElse TmElse = true
      | scheme_term_equal _ _ = false

    fun term_to_string (TmInteger anInt) = Int.toString anInt
      | term_to_string (TmBoolean false) = "#f"
      | term_to_string (TmBoolean true) = "#t"
      | term_to_string TmCons = "cons"
      | term_to_string TmCar = "car"
      | term_to_string TmCdr = "cdr"
      | term_to_string TmNull_p = "null?"
      | term_to_string TmEq_p = "eq?"
      | term_to_string TmAtom_p	= "atom?"		     
      | term_to_string TmZero_p = "zero?"
      | term_to_string TmSucc = "succ"
      | term_to_string TmPred = "pred"
      | term_to_string TmNumber_p = "number?"
      | term_to_string (TmIdentifier anIdentifier) = anIdentifier
      | term_to_string TmQuote = "quote"
      (* in this way we can use the output directly for evalution in
      Emacs, instead of the following commented code (which can be
      evaluated with Racket instead. *)
      (* | term_to_string TmLambda = "λ" *)
      | term_to_string TmLambda = "lambda" 
      | term_to_string TmCond = "cond"
      | term_to_string TmElse = "else"

    fun meaning_to_string (Quotation aSexp) = 
	(* "'" ^  *)    
	(SexpFunctions.to_string term_to_string aSexp)
      | meaning_to_string (Primitive aSexp) = 
	"Primitive function: " ^ 
	(SexpFunctions.to_string term_to_string aSexp)
      | meaning_to_string (NonPrimitive _) = 
	"Non primitive function"

    functor MakeInterpreter ()
	    :> SEXP_INTERPRETER where type term = scheme_term
	                        where type meaning = scheme_meaning
				where type 'a sexp = 'a Sexp.sexp
            =
	    struct

	        type 'a sexp = 'a Sexp.sexp

	        type term = scheme_term

		type meaning = scheme_meaning

		type identifier = Table.identifier

		type action = term Sexp.sexp 
			      -> scheme_meaning Table.table 
			      -> scheme_meaning

		exception EmptyListNotAllowedForNonPrimitiveExpression
		exception IdentifierNotBound of identifier
		exception Law_of_Cons
		exception Law_of_Car
		exception Law_of_Cdr
		exception Law_of_Null
		exception Law_of_Pred

		fun value aSexp = meaning_of aSexp Table.empty_table
		and meaning_of aSexp aTable = 
		    sexp_to_action aSexp aSexp aTable
		and sexp_to_action (atom as Sexp.Atom term) :action = 
		    (case term of
			 TmInteger _ => const_type
		       | TmBoolean _ => const_type
		       | TmCons => const_type
		       | TmCar => const_type
		       | TmCdr => const_type
		       | TmNull_p => const_type
		       | TmEq_p => const_type
		       | TmAtom_p => const_type
		       | TmZero_p => const_type
		       | TmSucc => const_type
		       | TmPred => const_type
		       | TmNumber_p => const_type
		       | TmIdentifier _ => identifier_type) 
		  | sexp_to_action (list as Sexp.List conses) =
		    case conses of
			Sexp.Cons (Sexp.Atom atom, _) =>
			let
			    fun A TmQuote = quote_type
			      | A TmLambda = lambda_type
			      | A TmCond = cond_type
			      | A _ = application_type
			in A atom end	    
		      | Sexp.Cons (_, _) => application_type
		      | Sexp.Null => 
			raise EmptyListNotAllowedForNonPrimitiveExpression
		and const_type (anInt as Sexp.Atom (TmInteger _)) _ = Quotation anInt
		  | const_type (aBoolean as Sexp.Atom (TmBoolean _)) _ = Quotation aBoolean
		  | const_type atom _ = Primitive atom
		and quote_type (Sexp.List 
				    (Sexp.Cons (
					  Sexp.Atom TmQuote, 
					  Sexp.Cons (aSexp, Sexp.Null)))) _ =
		    Quotation aSexp
		and identifier_type (Sexp.Atom (TmIdentifier key)) aTable = 
		    Table.lookup_in_table 
			(fn anotherKey => key = anotherKey)
			aTable
			(fn (Table.KeyNotFound _) => 
			    raise IdentifierNotBound key)
		and lambda_type (Sexp.List
				     (Sexp.Cons
					  (Sexp.Atom TmLambda, 
					   Sexp.Cons (
					       formals as Sexp.List _,
					       Sexp.Cons (
						   body as Sexp.List _,
						   Sexp.Null))))) 
				aTable = 
		    NonPrimitive {
			table=aTable, 
			formals=formals, 
			body=body
		    }
		and cond_type (Sexp.List
				   (Sexp.Cons
					(Sexp.Atom TmCond,
					 (* just to ensure that
					       there is at least one
					       question, namely
					       `TmElse' *)
					 Sexp.Cons (
					     Sexp.List lines, 
					     Sexp.Null))))
			      aTable = 
		    let
			fun evcon (Sexp.Cons (
					Sexp.List (
					    Sexp.Cons(
						question, 
						Sexp.Cons (answer, 
							   Sexp.Null))),
					other_questions)) =
			    case question of
				Sexp.Atom TmElse => meaning_of answer aTable
			      | _ => (case meaning_of question aTable of
					  Quotation (Sexp.Atom (TmBoolean true)) => 
					  meaning_of answer aTable
					| Quotation (Sexp.Atom (TmBoolean false)) => 
					  evcon other_questions)
		    in 
			evcon lines
		    end
		and application_type (Sexp.List 
					  (Sexp.Cons
					       (function,
						args)))
				     aTable = 
		    let 
			fun evlis Sexp.Null = []: scheme_meaning list
			  | evlis (Sexp.Cons (aSexp, other_args)) = 
			    (meaning_of aSexp aTable) :: (evlis other_args)

			(* cons ************************************************************* *)
			fun apply (Primitive (Sexp.Atom TmCons)) 
				  [Quotation carSexp, 
				   Quotation (Sexp.List cdr_slist)] = 
			    Quotation (Sexp.List (
					    Sexp.Cons (carSexp,
						       cdr_slist)))
			  | apply (Primitive (Sexp.Atom TmCons)) 
				  [Quotation carSexp, 
				   Quotation _] = 
			    raise Law_of_Cons

			  (* car ************************************************************ *)
			  | apply (Primitive (Sexp.Atom TmCar))
				  [Quotation (Sexp.List (
						   Sexp.Cons (car, _)))] =
			    Quotation car
			  | apply (Primitive (Sexp.Atom TmCar))
				  [Quotation _] = 
			    raise Law_of_Car

			  (* cdr ************************************************************ *)
			  | apply (Primitive (Sexp.Atom TmCdr))
				  [Quotation (Sexp.List (
						    (* we require that
						    the cdr isn't
						    Sexp.Null, asking
						    for at least
						    another Cons, in
						    order to not
						    violate `Law of
						    Cons'. *)
						    Sexp.Cons (_, cdr as Sexp.Cons (_, _))))] = 
			    Quotation (Sexp.List cdr)
			  | apply (Primitive (Sexp.Atom TmCdr))
				  [Quotation _] = 
			    raise Law_of_Cdr

			  (* null?************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmNull_p))
				  [Quotation (Sexp.List (aList))] = 
			    (case aList of
				Sexp.Null => Quotation (Sexp.Atom (TmBoolean true))
			      | Sexp.Cons (_,_) => Quotation (Sexp.Atom (TmBoolean false)))
			  | apply (Primitive (Sexp.Atom TmNull_p))
				  [Quotation _] = raise Law_of_Null

			  (* eq?**************************************************************** *)
			  (* the following are superflous due to the power of SexpFunction.equal function *)
			  (* | apply (Primitive (Sexp.Atom TmEq_p)) *)
			  (* 	  [Quotation (Sexp.Atom fst), Quotation (Sexp.Atom snd)] = *)
			  (*   (case (fst, snd) of *)
			  (* 	 (TmInteger n, TmInteger m) => *)
			  (* 	 Quotation (Sexp.Atom (TmBoolean (n = m))) *)
			  (*      | (TmBoolean true, TmBoolean true) => *)
			  (* 	 Quotation (Sexp.Atom (TmBoolean true)) *)
			  (*      | (TmBoolean false, TmBoolean false) => *)
			  (* 	 Quotation (Sexp.Atom (TmBoolean true)) *)
 			  (*      | (TmIdentifier fst_id, TmIdentifier snd_id) => *)
			  (*      	 Quotation (Sexp.Atom (TmBoolean (fst_id = snd_id))) *)
			  (*      | (_, _) => Quotation (Sexp.Atom (TmBoolean false))) *)
			  | apply (Primitive (Sexp.Atom TmEq_p))
				  (* PAY ATTENTION: the following
				   commented comment is wrong since
				   the `TmQuote' is elided during the
				   evaluation of arguments for an
				   application*)
				  (* [Quotation (Sexp.List ( *)
				  (* 		   Sexp.Cons ( *)
				  (* 		       Sexp.Atom TmQuote,  *)
				  (* 		       Sexp.Cons ( *)
				  (* 			   fst_sexp, *)
				  (* 			   Sexp.Null)))),  *)
				   (* Quotation (Sexp.List ( *)
				   (* 		   Sexp.Cons ( *)
				   (* 		       Sexp.Atom TmQuote,  *)
				   (* 		       Sexp.Cons ( *)
				   (* 			   snd_sexp, *)
				   (* 			   Sexp.Null))))] =  *)
				  [Quotation fst_sexp, Quotation snd_sexp] = 
			    Quotation (Sexp.Atom (TmBoolean (
						       SexpFunctions.equal scheme_term_equal 
									   fst_sexp 
									   snd_sexp)))

			  (* atom? **************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmAtom_p)) 
				  [atomic_meaning] =
			    (case atomic_meaning of
				 Quotation (Sexp.Atom (TmInteger _)) => 
				 Quotation (Sexp.Atom (TmBoolean true))
			       | Quotation (Sexp.Atom (TmBoolean _)) => 
				 Quotation (Sexp.Atom (TmBoolean true))
			       | Quotation (Sexp.Atom (TmIdentifier _)) => 
				 Quotation (Sexp.Atom (TmBoolean true))
			       (* the following rule is here only to
			       emphasize that the () list isn't an
			       atom, contrary to some lisp
			       implementation *)
			       | Quotation (Sexp.List Sexp.Null) => 
				 Quotation (Sexp.Atom (TmBoolean false))
			       | _ => Quotation (Sexp.Atom (TmBoolean false)))

			  (* zero? **************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmZero_p)) 
				  [Quotation (Sexp.Atom (TmInteger n))] =
			    (case n of 
				 0 => Quotation (Sexp.Atom (TmBoolean true))
			       | _ => Quotation (Sexp.Atom (TmBoolean false)))

			  (* succ ***************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmSucc)) 
				  [Quotation (Sexp.Atom (TmInteger n))] = 
			    Quotation (Sexp.Atom (TmInteger (n + 1)))

			  (* pred ***************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmPred))
				  [Quotation (Sexp.Atom (TmInteger n))] = 
			    (case n of 
				 0 => raise Law_of_Pred
			       | m => Quotation (Sexp.Atom (TmInteger (m-1))))

			  (* number? ************************************************************** *)
			  | apply (Primitive (Sexp.Atom TmNumber_p))
				  [Quotation (Sexp.Atom (TmInteger _))] = 
			    Quotation (Sexp.Atom (TmBoolean true))
			  | apply (Primitive (Sexp.Atom TmNumber_p))
				  [ _ ] = Quotation (Sexp.Atom (TmBoolean false))

			  (* non-primitive ******************************************************** *)
			  | apply (NonPrimitive {table, formals, body}) meanings = 
			    let 
				fun formals_to_identifiers_list Sexp.Null = []
				  | formals_to_identifiers_list 
					(Sexp.Cons (Sexp.Atom (TmIdentifier key), other_formals)) = 
				    key :: (formals_to_identifiers_list other_formals)

				val (Sexp.List formals_slist) = formals

				val new_entry = Table.new_entry 
						    (formals_to_identifiers_list formals_slist)
						    meanings

				val table' = Table.extend_table new_entry table
			    in
				meaning_of body table'
			    end
				
		    in 
			apply (meaning_of function aTable) (evlis args)
		    end
	    end
		

    end
