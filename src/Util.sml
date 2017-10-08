
structure Util =
struct
  infixr 1 $

  fun f $ a = f a

  fun fst (f,_) = f
  fun snd (_,s) = s
end

