
signature DYNAMIC =
sig
  type dyn
  val newdyn : unit -> ('a -> dyn) * (dyn -> 'a)
end

