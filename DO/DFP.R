library(numDeriv);
source("ONEDIM.R");

A = list();
grad_f_list = list();
k = 1;
A[[k]] = matrix(
            c(
              1, 0,
              0, 1
              ),
            nrow = 2, ncol = 2, TRUE
            );

x_values[[k]] = x0;
grad_f_list[[k]] = grad(f, x0);
f_values[k] = f(x_values[[k]]);
g = list();
delta_x = list();
S_vals = list();


while (k < 3) {
    S_vals[[k]] = A[[k]] %*% grad_f_list[[k]];
    lambda_ranges[[k]] = golden_cut(sven(S_vals[[k]]), S_vals[[k]]);
    lambda_values[k] = runif(1, lambda_ranges[[k]][1], lambda_ranges[[k]][2]);
    x_values[[k + 1]] = x_values[[k]] + lambda_values[k] * S_vals[[k]];
    f_values[k + 1] = f(x_values[[k + 1]]);
    grad_f_list[[k + 1]] = grad(f, x_values[[k + 1]]);
    g[[k]] = grad_f_list[[k + 1]] - grad_f_list[[k]];
    delta_x[[k]] = x_values[[k + 1]] - x_values[[k]];

    k = k + 1;
    A[[k]] = A[[k - 1]] + (delta_x[[k - 1]] %*% t(delta_x[[k - 1]]))[1, 1] / (t(delta_x[[k - 1]]) %*% g[[k - 1]])[1, 1] -
        (A[[k - 1]] %*% g[[k - 1]] %*% t(g[[k - 1]]) %*% A[[k - 1]])[1, 1] /
        (t(g[[k - 1]]) %*% A[[k - 1]] %*% g[[k - 1]])[1, 1];
}

print("Success!");