let rec score_aux frame throws = match throws with
    | [] -> 0
    | x :: y :: z :: zs when (x = 10) && (frame < 10) -> x + y + z + (score_aux (frame + 1) (y :: z :: zs))
    | x :: y :: z :: zs when (x + y = 10) && (frame < 10) -> x + y + z + (score_aux (frame + 1) (z :: zs))
    | x :: y :: zs -> x + y + (score_aux (frame + 1) zs)
    | x :: ys -> x + (score_aux frame ys)

let score throws = score_aux 1 throws

