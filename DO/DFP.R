library(numDeriv);

x0 = c(-1.2 * 8 - 5, -1.2 * 8 - 5);
x_values = list();
f_values = c();
epsilon = 0.001;
lambda_values = c();
lambda_ranges = list();

f = function(x) {
    return((x[1] - 8) ^ 2 - x[1] * x[2] + 3 * (x[2]) ^ 2);
}

fl = function(x, lambda, S) {
    cur_x = x + lambda * S;
    return(f(cur_x));
}

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

get_lambda = function(x, S) {
    hess = hessian(f, x);
    l = -1 * (t(grad(f, x)) %*% S) / (t(S) %*% hess %*% S);
    return(l[1, 1]);
}

while (k < 3) {
    S_vals[[k]] = -A[[k]] %*% grad_f_list[[k]];
    lambda_values[k] = get_lambda(x_values[[k]], S_vals[[k]]);
    x_values[[k + 1]] = x_values[[k]] + lambda_values[k] * S_vals[[k]];
    f_values[k + 1] = f(x_values[[k + 1]]);
    grad_f_list[[k + 1]] = grad(f, x_values[[k + 1]]);
    g[[k]] = grad_f_list[[k + 1]] - grad_f_list[[k]];
    delta_x[[k]] = x_values[[k + 1]] - x_values[[k]];

    k = k + 1;
    A_part_1 = (delta_x[[k - 1]] %*% t(delta_x[[k - 1]])) / (t(delta_x[[k - 1]]) %*% g[[k - 1]])[1, 1];
    A_part_2 = (A[[k - 1]] %*% g[[k - 1]] %*% t(g[[k - 1]]) %*% A[[k - 1]]) /
        (t(g[[k - 1]]) %*% A[[k - 1]] %*% g[[k - 1]])[1, 1];
    A[[k]] = A[[k - 1]] + A_part_1 - A_part_2;
}

print("Success!");