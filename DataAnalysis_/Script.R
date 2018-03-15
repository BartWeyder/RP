X = runif(1000);
j = 3;
Y = c();
for (i in 1:100) {
    Y[i] = X[j];
    j = j + 10;
}

print(mean(X));
print(mean(Y));

print(sd(X));
print(sd(Y));

y1 = mean(Y) - 1.96 * sqrt(sd(Y)) / sqrt(100);
y2 = mean(Y) + 1.96 * sqrt(sd(Y)) / sqrt(100);

print(y1);
print(y2);

X1 = runif(500);
X2 = runif(500, 0, 2);
X3 = runif(500, -0.5, 1.5);

X_united_1 = c(X1, X2);
X_united_2 = c(X1, X3);
j = 3;
Y_u_1 = c();
Y_u_2 = c();

for (i in 1:100) {
    Y_u_1[i] = X_united_1[j];
    Y_u_2[i] = X_united_2[j];
    j = j + 2;
}

print(mean(X_united_1));
print(mean(X_united_2));

y1 = mean(Y_u_1) - 1.96 * sqrt(sd(Y_u_1)) / sqrt(100);
y2 = mean(Y_u_1) + 1.96 * sqrt(sd(Y_u_1)) / sqrt(100);

print(y1);
print(y2);


y1 = mean(Y_u_2) - 1.96 * sqrt(sd(Y_u_2)) / sqrt(100);
y2 = mean(Y_u_2) + 1.96 * sqrt(sd(Y_u_2)) / sqrt(100);

print(y1);
print(y2);