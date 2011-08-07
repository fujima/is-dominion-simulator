let rec repeat f n x =
  match n with
    | 0 -> x
    | _ -> repeat f (n-1) (f x)
