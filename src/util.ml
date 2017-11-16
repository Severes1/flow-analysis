let intersect_diff x y =
  List.partition
    (fun z -> List.mem z y)
    x 

let intersect x y =
  let (intersection, _) = intersect_diff x y in
  intersection 

let diff x y =
  let (_, difference) = intersect_diff x y in
  difference
