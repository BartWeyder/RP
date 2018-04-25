library(numDeriv);
f = function(x) {
    return(3 * (x[1] ^ 2) + x[1] * x[2] + 2 * (x[2] ^ 2));
}

x0 = c(4, 4);

get_lambda = function(x, S) {
    hess = hessian(f, x);
    l = -1 * (t(grad(f, x)) %*% S) / (t(S) %*% hess %*% S);
    return(l[1, 1]);
}

#1 
S0 = c(0.667, 0.745);
lambda0 = get_lambda(x0, S0);
x1 = x0 + lambda0 * S0;

hessi = hessian(f, x0);
equation1 = t(S0) %*% hessi;
s1_ = (-equation1[2] / equation1[1]);
s12 = sqrt(1 / (1 + (s1_ ^ 2)));
s11 = s1_ * s12;
S1 = c(s11, s12);
lambda1 = get_lambda(x1, S1);
x2 = x1 + lambda1 * S1;
x2 = round(x2, 4);

#2 
S0 = -grad(f, x0);
lambda0 = get_lambda(x0, S0);
x1 = x0 + lambda0 * S0;
# seacrhing gamma0 for S1
gamma0 = (norm(grad(f, x1), type = "2") ^ 2) / (norm(grad(f, x0), type = "2") ^ 2);
S1 = -grad(f, x1) + gamma0 * S0;
lambda1 = get_lambda(x1, S1);
x2 = x1 + lambda1 * S1;
x2 = round(x2, 4);