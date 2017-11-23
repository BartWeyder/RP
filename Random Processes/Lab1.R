#Lab 1
# Task 1: Check uniform distribution
mn = runif(1000, 0, 1);
R = max(mn) - min(mn);
k = 10;
dx = R / k;
n = c();
v = c();
vt = c();
for (i in 1:k) {
    n[i] = length(mn[mn >= min(mn) + (i - 1) * dx & mn <= (min(mn) + i * dx)]);
    v[i] = n[i] / 1000;
    vt[i] = ((v[i] - dx) ^ 2) / dx;
}
sum(n);
sum(v);

xi2 = sum(vt);
Xi2 = qchisq(p = 0.95, df = 7);

if (xi2 > Xi2) { print("Reject the hypothesis"); } else { print("No reason to reject the hypothesis"); }
hist(mn, col = "lightblue");

# Task 2: check exponential distribution
library(exptest);
mn = rexp(1000);
ks.exp.test(mn);
lorenz.exp.test(mn);
dev.new();
hist(mn, col = "green");

# Task 3
for (i in 1:100) {
    x <- runif(1000);
    n[i] = max(x);
}
dev.new();
hist(n, col = "red");

# Task 4: individual. var 8
#RVals1 = array(c(runif(10000, 0, 14), runif(10000, 0, 2)), c(5000, 2));
N = 100000;
RIntX = runif(N, 0, 2);
RIntY = runif(N, 0, 14);

ctr = 0;
for (i in 1:N) {
    if (RIntY[i] <= RIntX[i] * ((RIntX[i] ^ 3) - 1)) {
        ctr = ctr + 1;
    }        
}

print(ctr / N * 14 * 2);
# expected result 22/5 = 4.4 (WolframAlpha)

#integral 2:
N = 100000;
RIntX1 = runif(N, 0, pi / 3);
RIntX2 = runif(N, pi / 3, pi / 2);
RIntY1 = runif(N, 0, 3);
RIntY2 = runif(N, -pi - 1, 0);
ctr1 = 0;
ctr2 = 0;
for (i in 1:N) {
    if (RIntY1[i] <= (2 * RIntX1[i] + 1) * sin(RIntX1[i] * 3)) {
        ctr1 = ctr1 + 1;
    }
    if (RIntY2[i] >= (2 * RIntX2[i] + 1) * sin(RIntX2[i] * 3)) {
        ctr2 = ctr2 + 1;
    }
}

square1 = ctr1 / N * pi;
square2 = ctr2 / N * pi / 6 * (pi + 1);
# Expected result 1/9 = 0.111 acording to WolframAlpha
print(square1 - square2);

#Integral 3
#analyticaly this integral = infinity

#TASK 5
#needed area drawn on paper
RIntX = runif(N, -1, 1);
RIntY = runif(N);
ctr = 0;
for (i in 1:N) {
    if ((RIntY[i] <= (-(RIntX[i] ^ 2) + 1)) && (RIntY[i] >= (RIntX[i] ^ 2))) {
        ctr = ctr + 1;
    }
}
# Expected result 2*sqrt(2)/3 = 0.942809 acording to WolframAlpha
print(ctr / N * 2);
