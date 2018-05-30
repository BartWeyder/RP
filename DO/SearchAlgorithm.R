# start search loop
# use counter later
counter = 0
epsilon = 0.01
# init gradaients
gradient_values0 = gradient(f, x0, h = grad_step)
# init A
A0 = matrix(  c(
  1, 0,
  0, 1
), nrow = 2, ncol = 2)
# init S list
S = list()
S0 = -gradient_values0
# init lambda values
lambda = list()
lambda0 = onedim_search(x0, S0, onedim_epsilon, type = golden_section)
# init x list
x = list(x0 + lambda0 * S0)
# init fs list
f_values = list(f(x[[1]]))
# init gradient values list
gradient_values = list(gradient(f, x[[1]], h = grad_step))
# init delta_g
delta_g0 = gradient_values[[1]] - gradient_values0
delta_g = list()
# init delta_x
delta_x0 = x[[1]] - x0
delta_x = list()

k = 1
A = list((A0 - (delta_x0 %*% t(delta_g0)) / (t(delta_x0) %*% delta_g0)[1, 1]) %*%
           A0 %*%
           (A0 - delta_x0 %*% t(delta_g0) / (t(delta_x0) %*% delta_g0)[1, 1]) +
           (delta_x0 %*% t(delta_x0) / (t(delta_x0) %*% delta_g0)[1, 1]))

while(TRUE)
{
  # repeating all algorithm from above
  S[[k]] = -A[[k]] %*% gradient_values[[k]]
  lambda[[k]] = onedim_search(x[[k]], S[[k]], onedim_epsilon, type = golden_section)
  if(lambda[[k]] < 0.001)
  {
    A[[k]] = A0
    S[[k]] = -gradient_values[[k]]
    lambda[[k]] = onedim_search(x[[k]], S[[k]], epsilon = onedim_epsilon, type = golden_section)
  }
  x[[k + 1]] = x[[k]] + lambda[[k]] * S[[k]]
  f_values[[k + 1]] = f(x[[k + 1]])
  gradient_values[[k + 1]] = gradient(f, x[[k + 1]], h = grad_step)
  delta_g[[k]] = gradient_values[[k + 1]] - gradient_values[[k]]
  delta_x[[k]] = x[[k + 1]] - x[[k]]
  
  if(
    # (norm(gradient_values[[k + 1]] - gradient_values[[k]], type = "2") <= epsilon_) &&
    (abs(f_values[[k + 1]] - f_values[[k]]) <= epsilon_) &
    (norm(x[[k + 1]] - x[[k]], type = "2") / norm(x[[k+1]], type = "2") <= epsilon_)
    )
      break()
  
  
  k = k + 1
  A[[k]] = (A0 - (delta_x[[k - 1]] %*% t(delta_g[[k - 1]])) /
              (t(delta_x[[k - 1]]) %*% delta_g[[k - 1]])[1, 1]) %*% A0 %*%
            (A0 - delta_x[[k - 1]] %*% t(delta_g[[k - 1]]) /
              (t(delta_x[[k - 1]]) %*% delta_g[[k - 1]])[1, 1]) +
              (delta_x[[k - 1]] %*% t(delta_x[[k - 1]]) /
              (t(delta_x[[k - 1]]) %*% delta_g[[k - 1]])[1, 1])
}