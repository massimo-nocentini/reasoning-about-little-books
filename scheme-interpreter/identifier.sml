signature IDENTIFIER = 
sig
    type identifier
end

functor MakeIdentifierAsString ()
	:> IDENTIFIER where type identifier = string
        =
	struct
	
	type identifier = string

	end
