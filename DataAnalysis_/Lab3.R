a = 0.95;
n = 1000;
delta = 0.95;
N = 8;
NG = 52;
x = c();
y = c();

for (i in 1:n) {
    x[i] = i + (runif(1) * N) / NG;
    y[i] = N * runif(1) * x[i] + NG * runif(1) + N;
}

# checking ejections
for (i in 1:n) {

}
