signature TABLE = 
sig

    type identifier
    type stuff

    type 'a keys
    type 'a values
    type entry

    val new_entry: identifier keys -> stuff values -> entry

    datatype lookup_failure = KeyNotFound of identifier -> bool
			    | ValuesNotEnough of stuff values

    type table
    val empty_table: table
    val extend_table: entry -> table -> table
    val lookup_in_table: (identifier -> bool) 
			 -> table 
			 -> (lookup_failure -> stuff)
			 -> stuff

end

functor MakeTableDoubleListImpl (
    structure IdentifierType: TYPE
    structure StuffType: TYPE)
	:> TABLE where type identifier = IdentifierType.aType
                 where type stuff = StuffType.aType
		 where type 'a values = 'a list
                 where type 'a keys = 'a list
        =
	struct

	type identifier = IdentifierType.aType
        type stuff = StuffType.aType

	type 'a values = 'a list
        type 'a keys = 'a list

	type entry = (identifier keys) * (stuff values)
	type table = entry list

	datatype lookup_failure = KeyNotFound of identifier -> bool
				| ValuesNotEnough of stuff values

	fun new_entry keys values = (keys, values)

	fun lookup_in_entry pred 
			    (anEntry as (orig_keys, orig_values))
			    aFailureHandler = 
	    let
		fun L ([], _) = aFailureHandler (KeyNotFound pred)
		  | L (aKey :: keys, []) = 
		    aFailureHandler (ValuesNotEnough orig_values)
		  | L (aKey :: keys, aValue :: values) = 
		    if pred aKey
		    then aValue
		    else L (keys, values)
	    in
		L anEntry
	    end
			   
	val empty_table = []
			      
	fun extend_table anEntry aTable = anEntry :: aTable

	fun lookup_in_table pred aTable aFailureHandler = 
	    let
		fun L [] = aFailureHandler (KeyNotFound pred)
		  | L (anEntry :: entries) = 
		    lookup_in_entry pred 
				    anEntry 
				    (failure_handler entries)
		and failure_handler entries (KeyNotFound _) = 
		    L entries
		  | failure_handler _ (vne as ValuesNotEnough _) =
		    aFailureHandler vne
	    in
		L aTable
	    end

	end
