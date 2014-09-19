
open SMLofNJ.Cont

fun storecc r th = callcc (fn k => (r := SOME k ; th ()))
val cont : int cont option ref = ref NONE;
val _ = print (Int.toString (1 + storecc cont (fn () => 0))) ;
(*val 2 =  throw (valOf (!cont)) 1 ;*)
val () = (print (Int.toString (1 + storecc cont (fn () => 0))) ; 
            case !cont of SOME k => (cont := NONE; throw k 1) | NONE => ())

