
structure Dynamic :> DYNAMIC =
struct
  exception Dynamic
  type dyn = exn

  fun newdyn () =
    let
      exception E of 'a
    in
      (E, fn (E a)=>a | _=> raise Dynamic)
    end
end

