
structure SchemeInterpreterEnvironment = 
    struct

    type identifier = string

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
			 | TmIdentifier of identifier
			 | TmQuote
			 | TmLambda
			 | TmCond
			 | TmElse

    functor MakeTypeSchemeTerm () 
	    :> TYPE where type aType = scheme_term
            =
	    struct type aType = scheme_term end

    structure Sexp = MakeSexp (
	structure ObjectType = MakeTypeSchemeTerm ())

    datatype scheme_meaning = Integer of int
			    | Quotation of Sexp.sexp
			    | Boolean of bool

    functor MakeInterpreter (
	structure Sexp: SEXP where type object = scheme_term
	val mk_quotation: Sexp.sexp -> scheme_meaning)
	(* val mk_bool_sexp: bool -> Sexp.sexp)  *)
	    :> SEXP_INTERPRETER where type term = scheme_term
	                        where type meaning = scheme_meaning
            =
	    struct

		structure Sexp = Sexp
		structure Table = MakeTableDoubleListImpl (
		    structure IdentifierType = MakeTypeString ())

	        type term = scheme_term

		(* meaning for the end of computation *)
		type meaning = scheme_meaning

		datatype computation_meaning = 
			 CPrimitive of Sexp.sexp
			 | CQuotation of Sexp.sexp
			 | CInteger of int
			 | CBoolean of bool
			 | CNonPrimitive of {
		       	     table: computation_meaning Table.table,
		       	     formals: Sexp.sexp,
		       	     body: Sexp.sexp
			 }
		       | CStub (* to delete when finished! *)

		type identifier = Table.identifier

		type action = Sexp.sexp 
			      -> computation_meaning Table.table 
			      -> computation_meaning

		exception EmptyListNotAllowedForNonPrimitiveExpression
		exception IdentifierNotBound of identifier


		fun value aSexp = 
		    case meaning_of aSexp Table.empty_table of
			CInteger i => Integer i
		      | CBoolean b => Boolean b
		      | CQuotation aSexp => mk_quotation aSexp
		and meaning_of aSexp aTable = 
		    sexp_to_action aSexp aSexp aTable
		and sexp_to_action (atom as Sexp.Atom term) = 
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
		       (* maybe here we can require the TmIdentifier
		       matching, leaving all the other cases to raise
		       an exception *)
		       | _ => identifier_type) 
		  | sexp_to_action (list as Sexp.List conses) =
		    case conses of
			Sexp.Cons (Sexp.Atom atom, _) =>
			let
			    fun A TmQuote = quote_type
			      | A TmLambda = lambda_type
			      | A TmCond = cond_type
			      (* maybe the following is too safe,
			      maybe better raising an exception *)
			      | A _ = application_type
			in A atom end	    
		      | Sexp.Cons (_, _) => application_type
		      | Sexp.Null => 
			raise EmptyListNotAllowedForNonPrimitiveExpression
		and const_type (Sexp.Atom (TmInteger i)) _ = CInteger i
		  | const_type (Sexp.Atom (TmBoolean b)) _ = CBoolean b
		  | const_type (atom as Sexp.Atom _) _ = CPrimitive atom
		and quote_type (Sexp.List 
				    (Sexp.Cons 
					 (Sexp.Atom TmQuote, conses))) _ =
		    CQuotation (Sexp.List conses)
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
		    CNonPrimitive {
			table=aTable, 
			formals=formals, 
			body=body
		    }
		and cond_type (Sexp.List
				   (Sexp.Cons
					(Sexp.Atom TmCond,
					 lines as Sexp.Cons
					       (* just to ensure that
					       there is at least one
					       question, namely
					       `TmElse' *)
					       (Sexp.List _,
						other_conses))))
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
					  CBoolean true => 
					  meaning_of answer aTable
					| CBoolean false => 
					  evcon other_questions)
		    in 
			evcon lines
		    end
		and application_type (Sexp.List 
					  (Sexp.Cons
					       (function as Sexp.List _,
						args)))
				     aTable = 
		    let 
			fun evlis Sexp.Null = []: computation_meaning list
			  | evlis (Sexp.Cons (aSexp, other_args)) = 
			    (meaning_of aSexp aTable) :: (evlis other_args)

			fun apply (CPrimitive (Sexp.Atom TmCons)) 
				  [car, cdr as CQuotation cdrSexp] = 
			    let
				val car_sexp = case car of
						   CInteger i => Sexp.Atom (TmInteger i)
						 | CBoolean b => Sexp.Atom (TmBoolean b)
						 | CQuotation aSexp => aSexp
			    in
				CQuotation (Sexp.List (
						 Sexp.Cons (car_sexp,
							    Sexp.Cons (cdrSexp, Sexp.Null))))
			    end
			  | apply (CPrimitive (Sexp.Atom TmCar))
				  [CQuotation (Sexp.List (
						    Sexp.Cons (car, _)))] =
			    CQuotation car
			  | apply (CPrimitive (Sexp.Atom TmCdr))
				  [CQuotation (Sexp.List (
						    (* we require that
						    the cdr isn't
						    Sexp.Null, asking
						    for at least
						    another Cons, in
						    order to not
						    violate `Law of
						    Cons'. *)
						    Sexp.Cons (_, cdr as Sexp.Cons (_, _))))] = 
			    CQuotation (Sexp.List cdr)
			  | apply (CPrimitive (Sexp.Atom TmNull_p))
				  [CQuotation (Sexp.List (aList))] = 
			    (case aList of
				Sexp.Null => CBoolean true
			      | Sexp.Cons (_,_) => CBoolean false)
			  | apply (CPrimitive (Sexp.Atom TmEq_p))
				  [fst, snd] = 
			    (case (fst, snd) of
				 (CInteger n, CInteger m) => CBoolean (n = m)
			       | (CBoolean true, CBoolean true) => CBoolean true
			       | (CBoolean false, CBoolean false) => CBoolean true
			       (* | (CQuotation q, CQuotation r) =>  CBoolean (q = r) *)
			       | (_, _) => CBoolean false)
			  | apply (CPrimitive (Sexp.Atom TmAtom_p)) 
				  [atomic_meaning] =
			    (case atomic_meaning of
				 CInteger _ => CBoolean true
			       | CBoolean _ => CBoolean true
			       | CQuotation (Sexp.List Sexp.Null) => CBoolean false
			       | CQuotation (Sexp.Atom _) => CBoolean true
			       | _ => CBoolean false) 
			  | apply (CPrimitive (Sexp.Atom TmZero_p)) 
				  [CInteger n] = 
			    (case n of 
				 0 => CBoolean true
			       | _ => CBoolean false)
			  | apply (CPrimitive (Sexp.Atom TmSucc)) 
				  [CInteger n] = CInteger (n + 1)
			  | apply (CPrimitive (Sexp.Atom TmPred)) 
				  [CInteger n] = 
			    (case n of 
				 0 => CInteger 0 (* here we should raise an exception instead *)
			       | n => CInteger (n-1))
			  | apply (CPrimitive (Sexp.Atom TmNumber_p)) 
				  [CInteger _] = CBoolean true
			  | apply (CPrimitive (Sexp.Atom TmNumber_p)) 
				  [_] = CBoolean false
			  | apply (CNonPrimitive {table, formals, body}) meanings = 
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
		


    structure Interpreter = MakeInterpreter (
	structure Sexp = Sexp
	(* simply, instead of (fn sexp_obj => Quotation sexp_obj) *)
	val mk_quotation = Quotation)

    end
