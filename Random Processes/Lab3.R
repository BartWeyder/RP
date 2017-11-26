#Lab 3. Poisson process
# Task 1:
lambda = 4;
X = rpois(10000, lambda = lambda);
hist(X);
poisson.test(sum(X), length(X), lambda);

# Task 2:
dev.new();
n = 30
lambda1 = 2.4
lambda2 = 0.6
s1 = 0
s2 = 0
for (i in 1:n) {
    s1[i + 1] = s1[i] + sum(rpois(n = 1, lambda1))
    s2[i + 1] = s2[i] + sum(rpois(n = 1, lambda1))
}