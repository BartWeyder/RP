# 1) Generating data
n = 1000;
N = 8;
k = 5;
first_table = matrix(nrow = n, ncol = k);
multiplier = c(1, 0.5, 0.8, 1.4, 2);
for (i in 1:k) {
    for (j in 1:n) {
        first_table[j, i] = multiplier[i] * N + runif(1);
    }
}

# search for sd:
S_onefact_array = c();
for (i in 1:k) {
    S_onefact_array[i] = sd(first_table[, i]) * n / (n - 1);
}

# g value
g = max(S_onefact_array) / sum(S_onefact_array);

# calculating S02:
# first sum:
xij2_sum = 0;
for (i in 1:k) {
    for (j in 1:n) {
        xij2_sum = xij2_sum + first_table[j, i] ^ 2;
    }
}

# second sum:
sum_of_sqrs = 0;
for (i in 1:k) {
    xij = 0;
    for (j in 1:n) {
        xij = xij + first_table[j, i];
    }
    sum_of_sqrs = sum_of_sqrs + xij ^ 2;
}
sum_of_sqrs = sum_of_sqrs / n;

# and last step:
S02 = 1 / (k * (n - 1)) * (xij2_sum - sum_of_sqrs);

# calculating S2:
# xij2_sum we have, so calculate sqr of elements sum:
sqr_of_sum = 0;
for (i in 1:k) {
    for (j in 1:n) {
        sqr_of_sum = sqr_of_sum + first_table[j, i];
    }
}
sqr_of_sum = sqr_of_sum ^ 2;

# last step:
S2 = 1 / (k * n - 1) * (xij2_sum - sqr_of_sum / (k * n));

# calculating SA2
xi_ = c();
for (i in 1:k) {
    xi_[i] = mean(first_table[, i]);
}
x__ = mean(xi_);
sum_for_calc = 0;
for (i in 1:5) {
    sum_for_calc = sum_for_calc + (xi_[i] - x__) ^ 2;
}
SA2 = n / (k - 1) * sum_for_calc;
#??????
print(SA2 / S02);

# twofact:
k = 5;
m = 4;
n = 100;
data = array(dim = c(m, k, n));
rand_multiplier = c(1, 3.5, 3.8, 1.4, 2);
incr_for_multer = c(0, -1, -1, 1, 1);
# generating values:
data_mid = matrix(nrow = m, ncol = k);
Q1 = 0;
for (i in 1:m) {
    for (j in 1:k) {
        data[i, j,] = runif(n) + rand_multiplier[j] * N;
        data_mid[i, j] = mean(data[i, j,]);
        Q1 = Q1 + data_mid[i, j] ^ 2;
        rand_multiplier[j] = rand_multiplier[j] + incr_for_multer[j];
    }
}

Q2 = 0;
Q4 = 0;
for (i in 1:k) {
    Q2 = Q2 + sum(data_mid[, i]) ^ 2;
    Q4 = Q4 + sum(data_mid[, i]);
}
Q2 = Q2 / m;
Q4 = (Q4 ^ 2) / (m * k);

Q3 = 0;
for (i in 1:m) {
    Q3 = Q3 + sum(data_mid[i,]) ^ 2;
}
Q3 = Q3 / k;

S02_2f = (Q1 + Q4 - Q2 - Q3) / ((k - 1) * (m - 1));
SA2_2f = (Q2 - Q4) / (k - 1);
SB2_2f = (Q3 - Q4) / (m - 1);

#
f1a = k - 1;
f2a = (k - 1) * (m - 1);
summary_a_single = SA2_2f / S02_2f;

#
f1b = m - 1;
f2b = f2a;

Q5 = 0;
for (i in 1:m) {
    for (j in 1:k) {
        for (v in 1:n) {
            Q5 = Q5 + data[i, j, v] ^ 2;
        }
    }
}

SAB2 = (Q5 - n * Q1) / (m * k * (n - 1));
fab1 = f2a;
fab2 = m * k * (n - 1);
summary_ab = n * S02_2f / SAB2;
