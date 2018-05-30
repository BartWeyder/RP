a = 2
b = 2
c = 3

x0 = c(3, 4, 5)
s1 = c(1, 0, 0)
s2 = c(0, 1, 0)
s3 = c(0, 0, 1)

f = function(x)
{
  return(a * (x[1] ^ 2) + x[1] * x[2] + x[2] * x[3] + b * (x[2] ^ 2) 
         + c * (x[3] ^ 2))
}

get_lambda = function(x, S) 
{
  hess = hessian(f, x);
  l = -1 * (t(grad(f, x)) %*% S) / (t(S) %*% hess %*% S);
  return(l[1, 1]);
}

x = list()
x[[1]] = x0 + lambdas[1] * s3
lambdas[2] = get_lambda(x[[1]], s1)
x[[2]] = x[[1]] + lambdas[2] * s1
lambdas[3] = get_lambda(x[[2]], s2)
x[[3]] = x[[2]] + lambdas[3] * s2
lambdas[4] = get_lambda(x[[3]], s3)
x[[4]] = x[[3]] + lambdas[4] * s3
s4 = x[[4]] - x[[1]]
lambdas[5] = get_lambda(x[[4]], s4)
x[[5]] = x[[4]] + lambdas[5] * s4