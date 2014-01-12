signature TYPE = 
sig
    type aType
end

functor MakeTypeString ()
	:> TYPE where type aType = string
        =
	struct
	
	type aType = string

	end
