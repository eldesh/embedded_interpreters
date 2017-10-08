
structure Util =
struct
  infixr 1 $

  fun f $ a = f a

  fun id x = x

  fun fst (f,_) = f
  fun snd (_,s) = s

  fun const x _ = x
end

