open Owl 
open Algodiff.D

let rec gauss_newton ?(eps=1e-6) ?(maxiter=-1) f guess = 
  if maxiter == 0 then (guess, loss) else (
    let r, j_r = jacobianv f guess in 
    let loss = Maths.l2norm r |> unpack_flt in 
    if loss < eps then (guess, loss) else (
      let j_r_t = transpose j_r in 
      let new_guess = guess -. (sqrt (j_r_t *. j_r)) *. j_r_t *. r *. guess in 
      gauss_newton eps (maxiter - 1) f new_guess
    )
  )
