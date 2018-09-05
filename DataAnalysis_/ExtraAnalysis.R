# @author Yevhenii Kizim
# Variant: 1
require(ggplot2)
# read data 
input_data = read.delim("ff1.txt", header = FALSE)

# Task 1:
# 1.1 Build graphic
plot(input_data$V1)
# 1.2 Mean
mean_value = mean(input_data$V1)
# 1.3 Deviation
S2 = sd(input_data$V1)
# 1.4 Histogram
hist(input_data$V1)
# 1.5 Norming data
input_data$Vnorm = (input_data$V1 - mean_value) / sqrt(S2);
# 1.6 checking autocorrelation
acf(input_data$Vnorm, 5000)

# Task 2 Deviation analysis
# 2.1 graphic analysis
# Approximatelly every 4000 elements graphic changes his behaviour.
# So predict that we have two factors with 4 levels each.
# 2.2.1 Init data array
n = 4
chunk_length = length(input_data$V1) / (n * n)
cur_offset = 1
data_fact = array(dim = c(n, n, chunk_length))
data_mid = matrix(nrow = n, ncol = n);
# formulae part
Q = c(0, 0, 0, 0, 0)

for (i in 1:n) {
    for (j in 1:n) {
        data_fact[i, j,] = input_data$V1[cur_offset:(cur_offset + chunk_length - 1)]
        data_mid[i, j] = mean(data_fact[i, j,])
        Q[1] = Q[1] + data_mid[i, j] ^ 2
        cur_offset = cur_offset + chunk_length
    }
}

for (i in 1:n) {
    Q[2] = Q[2] + sum(data_mid[, i]) ^ 2
    Q[4] = Q[4] + sum(data_mid[, i])

}
Q[2] = Q[2] / n
Q[4] = (Q[4] ^ 2) / (n * n)

for (i in 1:n) {
    Q[3] = Q[3] + sum(data_mid[i,]) ^ 2
}
Q[3] = Q[3] / n

S02_2f = (Q[1] + Q[4] - Q[2] - Q[3]) / ((n - 1) * (n - 1))
SA2_2f = (Q[2] - Q[4]) / (n - 1)
SB2_2f = (Q[3] - Q[4]) / (n - 1)

f1 = n - 1
f2 = (n - 1) * (n - 1)
# F(3,9) = 3.86
summary_a_single = SA2_2f / S02_2f
summary_b_single = SB2_2f / S02_2f

Q[5] = sum(data_fact ^ 2)
SAB2 = (Q[5] - chunk_length * Q[1]) / (n * n * (chunk_length - 1))
summary_ab = chunk_length * S02_2f / SAB2
f1_ab = f2
f2_ab = n * n * (chunk_length - 1)
# F ~~ 1.88

# Task 3 Regression analysis
x = 1:length(input_data$V1)
regr_data = as.data.frame(x = x)
n = length(input_data$V1)
# linear
lin_model = lm(input_data$V1 ~ x)
regr_data$y = input_data$V1
regr_data$fit = lin_model$fitted.values
# check
e = input_data$V1 - lin_model$fitted.values
si_2 = sum(e ^ 2) / (n - 2) * (1 - 1 / n - (x - mean(x)) ^ 2 / sum((x - mean(x)) ^ 2))
R = max(abs(e / sqrt(si_2)))
while (R > 4) {
    cleared_data = c(0)
    cleared_fit = c(0)
    cleared_x = c(0)
    j = 1
    x = c(0)
    for (i in 1:n) {
        if (abs(e[i] / sqrt(si_2[i])) < 4) {
            cleared_data[j] = regr_data$y[i]
            cleared_fit[j] = regr_data$fit[i]
            x[j] = regr_data$x[i]
            j = j + 1
        }
    }
    regr_data = as.data.frame(x = x)
    regr_data$fit = cleared_fit
    regr_data$y = cleared_data
    n = length(regr_data$x)
    e = cleared_data - cleared_fit
    si_2 = sum(e ^ 2) / (n - 2) * (-(x - mean(x)) ^ 2 / sum((x - mean(x)) ^ 2) + 1 - 1 / n)
    R = max(abs(e / sqrt(si_2)))
}

# H0: beta == b
sx_2 <- sum((regr_data$x - mean(regr_data$x)) ^ 2) / (n - 1)
s_2 <- sum((regr_data$y - regr_data$fit) ^ 2) / (n - 2)
s_beta <- sqrt(s_2 / (sx_2 * (n - 1)))
t <- 1.645

if (abs(lin_model$coefficients[2]) > t * s_beta) {
    print("coeficient b is significant ")
} else {
    print("coeficient b is not significant ")
}

# H0: alpha == a
F_delta <- 1.03
s_alpha <- sqrt(s_2 * (1 / n + mean(regr_data$x) ^ 2 / ((n - 1) * sx_2)))
if (abs(lin_model$coefficients[1]) > t * s_alpha) {
    print("coeficient a is significant ")
} else {
    print("coeficient a is not significant ")
}

#H0: model is adequate
sy_2 <- sum((regr_data$y - mean(regr_data$y)) ^ 2) / (n - 1)
if (s_2 / sy_2 < F_delta) {
    print("the model is adequate")
} else {
    print("the model is not adequate")
}

sqr_model = lm(input_data$V1 ~ x + I(x ^ 2))
cub_model = lm(input_data$V1 ~ x + I(x ^ 2) + I(x ^ 3))
ggplot() + geom_point(aes(x = x, y = input_data$V1)) +
    geom_line(aes(x, lin_model$coefficients[1] + lin_model$coefficients[2] * x),
    col = "red") +
    geom_line(aes(x, sqr_model$coefficients[1] + sqr_model$coefficients[2] * x +
        sqr_model$coefficients[3] * x ^ 2), col = "green") +
    geom_line(aes(x, cub_model$coefficients[1] + cub_model$coefficients[2] * x +
        cub_model$coefficients[3] * x ^ 2 + cub_model$coefficients[4] * x ^ 3), col = "blue")

# sin-cos part
ssp = spectrum(regr_data$y)
per = 1 / ssp$freq[ssp$spec == max(ssp$spec)]
#per = 333.3333
sin_cos_model = lm(regr_data$y ~
                           (sin(2 * pi / per * regr_data$x)
                            + cos(2 * pi / per * regr_data$x)))

plot(regr_data$y ~ regr_data$x)
lines(fitted(sin_cos_model) ~ regr_data$x, col = 4)

# partial sin-cos
part_length = 1000
ssp = spectrum(regr_data$y[1:part_length])
per = 1 / ssp$freq[ssp$spec == max(ssp$spec)]
#per = 333.3333
sin_cos_model_partial = lm(regr_data$y[1:part_length] ~
                           (sin(2 * pi / per * regr_data$x[1:part_length])
                            + cos(2 * pi / per * regr_data$x[1:part_length])))

plot(regr_data$y[1:part_length] ~ regr_data$x[1:part_length])
lines(fitted(sin_cos_model_partial) ~ regr_data$x[1:part_length], col = 4)


# Task 4 - Fourier analysis
# Giving graphic of spectre (autoregressived)
sp = spectrum(input_data$V1, method = "ar")
# sp = spectrum(input_data$V1)
per = 1 / sp$freq[sp$spec == max(sp$spec)]
input_data$fourier = fft(input_data$V1)

ggplot() +
    geom_line(aes(x = x,
    y = Re(input_data$fourier) * cos(2 * pi * x / per) -
        Im(input_data$fourier) * sin(2 * pi * x / per)))

# Task 5 - Wavelet analysis
library(rgl)
#N = length(input_data$V1)
N = 128
M1 = N
is_gauss = FALSE
temp = c()

g1 = function(t) {
    if (is_gauss)
        return(exp(-(t ^ 2) / 2))
    else {
        if (0 <= t && t <= 1 / 2)
            return(1)
        else if (1 / 2 <= t && t <= 1)
            return(-1)
        else
            return(0)
        }
}

f2 = function(j, k, x) {
    return(2 ^ (j / 2) * g1((2 ^ j) * x - k))
}

W = function(l, j) {
    temp = c()
    for (i in 0:(N - 1)) {
        temp[i + 1] = smp[i + 1] * f2(l, j, i)
    }
    return(sum(temp))
}

pr = function(i, l) {
    temp = c()
    for (j in 0:M1) {
        temp[j + 1] = W(l, j) * f2(l, j, i) / (2 ^ (2 * l))
    }
    return(sum(temp))
}

d = function(i) {
    temp = c()
    for (l in 0:N) {
        temp[l + 1] = pr(i, l)
    }
    return(sum(temp))
}

d_ = c()
#N = 128
for (i in 0:(N - 1)) {
    d_[i + 1] = d(i)
}

ggplot() + geom_line(aes(x = 1:length(d_), y = d_)) + geom_line(aes(x = 1:N, y = smp), col = "red")

ggplot() + geom_line(aes(x = 1:length(d_), y = d_ / 3)) + geom_line(aes(x = 1:N, y = smp), col = "red")

j = 1:M1
l = 1:10
z = matrix(nrow = length(l), ncol = length(j))
for (i in l) {
    for (k in j) {
        z[i, k] = W(i, k)
    }
}

rgl = persp3d(l, j, z, col = "grey", alpha = 0.7)


#require(WaveletComp)
#wv_an = analyze.wavelet(input_data, "V1",
#loess.span = 0,
#dt = 1, dj = 1 / 250,
#lowerPeriod = 16,
#upperPeriod = 128,
#make.pval = TRUE, n.sim = 10)

#wt.image(wv_an, color.key = "quantile", n.levels = 250,
#legend.params = list(lab = "wavelet power levels", mar = 4.7))
