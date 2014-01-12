signature TEMP = sig end

(* signature SCHEME_INTERPRETER = *)
(* sig *)
(*     structure Sexp : SEXP *)

(*     eqtype entry_identifier *)
(*     (* type 'a entry = (entry_identifier * 'a) list *) *)
(*     type 'a entry = entry_identifier list * 'a list *)
(*     val lookup_in_entry: entry_identifier  *)
(* 			 -> 'a entry *)
(* 			 -> (entry_identifier -> 'a)  *)
(* 			 -> 'a *)

(*     type 'a table = 'a entry list *)
(*     val extend_table: 'a table -> 'a table *)
(*     val lookup_in_table: ('a -> bool)  (* instead of entry_identifier  *) *)
(* 			 -> 'a table *)
(* 			 -> (entry_identifier -> 'a) *)
(* 			 -> 'a *)

(*     datatype language_values = Integer of int *)
(* 			     | Boolean of bool *)
(* 			     | Quotation of expression Sexp.sexp *)
(* 	 and expression = Value of language_values *)
(* 			| cons | car | cdr | nullp | eqp | atomp | zerop *)
(* 			| succ | pred  *)

(*     datatype to_eval = Primitive of 'a Sexp.sexp *)
(*     type 'a reducee = 'a Sexp.sexp -> 'a table *)
(*     type 'a action = reducee -> reduced *)

(*     val expression_to_action: 'a Sexp.sexp -> 'a action *)
(*     val meaning: 'a reducee -> reduced *)
(*     val value: 'a Sexp.sexp -> reduced *)

(*     val const_type: 'a action *)



(* end *)
