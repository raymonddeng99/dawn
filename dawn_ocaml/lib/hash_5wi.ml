open Base

let universal_hash (a, b, c, d, e) key =
  let m = 2 ^ 32 in
  (a * key + b * key * key + c * key * key * key + d * key * key * key * key + e * key * key * key * key * key) mod m

let linear_probe table key =
  let m = Array.length table in
  let hash = universal_hash (random_params ()) key in
  let rec probe i =
    let index = (hash + i) mod m in
    if table.(index) = None then index
    else probe (i + 1)
  in probe 0