
let rec score throws = match throws with
| [] -> 0
| x :: y :: z :: zs when x = 10 -> x + y + z + score (y :: z :: zs)
| x :: y :: z :: zs when x + y = 10 -> x + y + z + score (z :: zs)
| x:: xs -> x + (score xs)
