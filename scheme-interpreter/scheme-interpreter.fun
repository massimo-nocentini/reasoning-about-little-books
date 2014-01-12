
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
		       | _ => identifier_type)
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
			fun is_else_question (Sexp.Atom TmElse) = true
			  | is_else_question _ = false
						     
			fun evcon (Sexp.Cons (
					Sexp.List (
					    Sexp.Cons(
						question, 
						Sexp.Cons (answer, 
							   Sexp.Null))),
					other_questions)) =
			    if is_else_question question
			    then meaning_of answer aTable
			    else case meaning_of question aTable of
				     CBoolean true => 
				     meaning_of answer aTable
				   | CBoolean false => 
				     evcon other_questions
		    in 
			evcon lines
		    end
		and application_type aSexp aTable = CStub



	    end
		

    structure Interpreter = MakeInterpreter (
	structure Sexp = Sexp
	(* simply, instead of (fn sexp_obj => Quotation sexp_obj) *)
	val mk_quotation = Quotation)

    end
