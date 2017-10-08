
structure Either =
struct
  datatype ('a, 'b) t = Right of 'a
                      | Left of 'b
end

