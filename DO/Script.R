install.packages("numDeriv");
library(numDeriv);

f = function(arg) {
    return(2 * arg[1] * arg[1] + 2 * arg[1] * arg[2] + 3 * arg[2] * arg[2]);
}

x0 = c(6, 4);
l = 1;
LS0 = -1 * grad(f, x0);
LX1 = x0 + LS0;
LS1 = -1 * grad(f, LX1);
LX2 = LX1 + LS1;
LS2 = -1 * grad(f, LX2);
LX3 = LX2 + LS2;

print("-----1------");
print(LS0);
print(LS1);
print(LS2);
print(LX1);
print(LX2);
print(LX3);

# 2
LOpt0 = -1 * (t(grad(f, x0)) %*% LS0) / (t(LS0) %*% hessian(f, x0) %*% LS0);
LoX1 = x0 + LOpt0[1, 1] * LS0;
LOpt1 = -1 * (t(grad(f, LoX1)) %*% LS1) / (t(LS1) %*% hessian(f, LoX1) %*% LS1);
LoX2 = LoX1 + LOpt1[1, 1] * LS1;
LOpt2 = -1 * (t(grad(f, LoX2)) %*% LS2) / (t(LS2) %*% hessian(f, LoX2) %*% LS2);
LoX3 = LoX2 + LOpt2[1, 1] * LS2;

print("-----2------");
print(LS0);
print(LS1);
print(LS2);
print(LOpt0);
print(LOpt1);
print(LOpt2);
print(LoX1);
print(LoX2);
print(LoX3);

# 4
NX1 = x0 - solve(hessian(f, x0)) %*% grad(f, x0);
NX2 = NX1 - solve(hessian(f, NX1)) %*% grad(f, NX1);
NX3 = NX2 - solve(hessian(f, NX2)) %*% grad(f, NX2);
print(NX1);
print(NX2);
print(NX3);

#3

LOpt_new = -1 * (t(grad(f, LoX2)) %*% (LoX2 - x0)) / (t(LoX2 - x0) %*% hessian(f, LoX2) %*% (LoX2 - x0));
X3 = LoX2 + LOpt_new[1, 1] * (LoX2 - x0);