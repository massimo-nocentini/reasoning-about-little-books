
structure SchemeInterpreterEnvironment = 
    struct

    type identifier = string

    datatype scheme_term = integer of int
			 | boolean of bool
			 | cons
			 | car 
			 | cdr
			 | null_p
			 | eq_p
			 | atom_p				     
			 | zero_p
			 | succ
			 | pred
			 | number_p
			 | identifier of identifier
			 | quote
			 | lambda
			 | cond
			 | elseq

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
			 integer _ => const_type
		       | boolean _ => const_type
		       | cons => const_type
		       | car => const_type
		       | cdr => const_type
		       | null_p => const_type
		       | eq_p => const_type
		       | atom_p => const_type		     
		       | zero_p => const_type
		       | succ => const_type
		       | pred => const_type
		       | number_p => const_type
		       | _ => identifier_type)
		  | sexp_to_action (list as Sexp.List conses) =
		    case conses of
			Sexp.Cons (Sexp.Atom atom, _) =>
			let
			    fun A quote = quote_type
			      | A lambda = lambda_type
			      | A cond = cond_type
			      | A _ = application_type
			in A atom end	    
		      | Sexp.Cons (_, _) => application_type
		      | Sexp.Null => 
			raise EmptyListNotAllowedForNonPrimitiveExpression
		and const_type (Sexp.Atom (integer i)) _ = CInteger i
		  | const_type (Sexp.Atom (boolean b)) _ = CBoolean b
		  | const_type (atom as Sexp.Atom _) _ = CPrimitive atom
		and quote_type (Sexp.List 
				    (Sexp.Cons 
					 (Sexp.Atom quote, conses))) _ =
		    CQuotation (Sexp.List conses)
		and identifier_type (Sexp.Atom (identifier key)) aTable = 
		    Table.lookup_in_table 
			(fn anotherKey => key = anotherKey)
			aTable
			(fn (Table.KeyNotFound _) => 
			    raise IdentifierNotBound key)
		and lambda_type (Sexp.List
				     (Sexp.Cons
					  (Sexp.Atom lambda, 
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
					(Sexp.Atom cons,
					 lines as Sexp.Cons
					       (* just to ensure that
					       there is at least one
					       question, namely
					       `elseq' *)
					       (Sexp.List _,
						other_conses))))
			      aTable = 
		    let
			fun is_else_question (Sexp.Atom elseq) = true
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
