N = 1000;
Population = runif(N, 29, 90);
M = 100;
X = c();
j = 1;
for (i in 1:M) {
    X[i] = Population[j];
    j = j + 10;
}

X = sample(Population, M);

# Task A
print("Population Mean:");
print(mean(Population));
print("Population Disp:");
print(sd(Population) * N / (N - 1));

# Task B
#sum
Y = 0;
for (i in 1:M) {
    Y = Y + X[i] / M;
}
Y = N * Y;
print("Sum X:")
print(Y);

# mean X
print("Mean X:");
Ys = mean(X);
print(Ys);
print("Disp X:");
S2 = sd(X) * M / (M - 1);
print(S2);

# Disp of sum rate
print("D(Ys):");
DYs = S2 / M * (1 - M / N);

# Disp of mean
print("D(Y):");
DY = S2 * N * N / M * (1 - M / N);

# mean limits
upper_limit_mean = Ys + (1.96 * sqrt(S2)) / M * sqrt(1 - M / N);
lower_limit_mean = Ys - (1.96 * sqrt(S2)) / M * sqrt(1 - M / N);
print("Mean lower limit:");
print(lower_limit_mean);
print("Mean upper limit:");
print(upper_limit_mean);

# sum limits
upper_limit_sum = N * Ys + (1.96 * sqrt(S2) * N) / sqrt(M) * sqrt(1 - M / N);
lower_limit_sum = N * Ys - (1.96 * sqrt(S2) * N) / sqrt(M) * sqrt(1 - M / N);
print("Sum lower limit:");
print(lower_limit_sum);
print("Sum upper limit:");
print(upper_limit_sum);

# Task D
Y = list(c(),c(),c());
for (i in 1:200) {
    Y[[1]][i] = Population[i];
}
j = 1;
for (i in 201:700) {
    Y[[2]][j] = Population[i];
    j = j + 1;
}
j = 1;
for (i in 701:1000) {
    Y[[3]][j] = Population[i];
    j = j + 1;
}

y = list();
for (i in 1:3) {
    y[[i]] = sample(Y[[i]], length(Y[[i]]) / 10);
}

# weight
w = c();
yh = c();
Yh = c();
Sh2 = c();
W = c();
Yhs = c();
yhs = c();
sh2 = c();

# sum of popualtion
Ysum = sum(Population);
# mean for population
yu = mean(Population);

for (i in 1:3) {
    w[i] = length(Y[[i]]) / length(y[[i]]);
    # mean value
    yh[i] = mean(Y[[i]]);
    # sum value
    Yh[i] = sum(Y[[i]]);
    # Disp in every strat
    Sh2[i] = sd(Y[[i]]) * length(Y[[i]]) / (length(Y[[i]]) - 1);
    # weight of strats
    W[i] = length(Y[[i]]) / N;
    # mean small strat
    yhs[i] = mean(y[[i]]);
    Yhs[i] = length(Y[[i]]) / length(y[[i]]) * sum(y[[i]]);
    sh2[i] = sd(y[[i]]) * length(y[[i]]) / (length(y[[i]]) - 1);
}

Yst = sum(Yhs);
Yst_small = 0;
yst = Yst / N;
yst_small = 0;
for (i in 1:3) {
    Yst_small = Yst_small + length(Y[[i]]) * yhs[i];
    yst_small = yst_small + W[i] * yhs[i];
}

DYst = 0;
Dyst = 0;
for (i in 1:3) {
    DYst = DYst + (1 - length(y[[i]]) / length(Y[[i]])) *
        length(Y[[i]]) * length(Y[[i]]) * sh2 / length(y[[i]]);

    Dyst = Dyst + (1 - length(y[[i]]) / length(Y[[i]])) *
        ((length(Y[[i]]) / N) ^ 2) * sh2 / length(y[[i]]);
}

Dyst_short = 1 / (N ^ 2) * DYst;

yst_upper = 1.96 * sqrt(Dyst);
yst_lower = -1 * yst_upper;

print("Weight:");
print(w);
print("Mean for full strat:");
print(yh);
print("Sum for full strat:");
print(Yh);
print("Sum for full Population:");
print(Ysum);
print("Mean for full Population:");
print(yu);
print("Disp for full strat");
print(Sh2);
print("Weight of strat");
print(W);
print("General sum rate:")
print(Yhs);
print("Mean rate for stat:")
print(yhs);
print("Rate for small disp:");
print(sh2);

print("Full SUM:");
print(Yst);
print(Yst_small);

print("Mean full:");
print(yst);
print(yst_small);

print("Nezmis4ena disp big:");
print(DYst);

print("Small:");
print(Dyst);
print(Dyst_short);

print("upper limit:");
print(yst_upper);
print("lower:");
print(yst_lower);