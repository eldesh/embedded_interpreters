
signature EMBED =
sig
  type 'a EP
  datatype U = datatype Univ.U
  val embed   : 'a EP -> 'a -> U
  val project : 'a EP -> U -> 'a
  val eptypeof : 'a EP -> EpType.t

  val S : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  val K : 'a -> 'b -> 'a
  val I : 'a -> 'a

  val unit   : unit EP
  val bool   : bool EP
  val int    : int  EP
  val string : string EP
  val any    : U EP (* mapped to U itself for polymorphism *)
  val list   : 'a EP -> 'a list EP
  val **     : ('a EP) * ('b EP) -> ('a*'b) EP
  val -->    : ('a EP) * ('b EP) -> ('a->'b) EP
  val wrap   : ('a->'b) * ('b->'a) -> 'b EP -> 'a EP
  val sum    : 'a EP list -> 'a EP
  val mu     : ('a EP -> 'a EP) -> 'a EP

  val newtype : unit -> 'a EP
end

