external id : 'a -> 'a = "%identity"
external apply : ('a -> 'b) -> 'a -> 'b = "%apply"
external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"

let flip f = fun a b -> f b a

let replicate c v =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (v :: acc) (n - 1)
    in
    loop [] c

let zipWith f xs ys =
    let rec loop = function
      | ([], _) -> []
      | (_, []) -> []
      | ((x :: xs), (y :: ys)) -> f x y :: loop (xs, ys)
    in
    loop (xs, ys)
