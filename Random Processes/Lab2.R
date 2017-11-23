#initial: choose library to test rnorm
library(nortest);
#get 1000 random values
RandomValue = rnorm(1000);
#check with different test
# Lilliefors (Kolmogorov-Smirnov) normality test
lillie.test(RandomValue);
# Anderson-Darling normality test
ad.test(RandomValue);
# Pearson chi-square normality test
pearson.test(RandomValue);
# Cramer-von Mises normality test
cvm.test(RandomValue);

#get Hist of random values:
hist(RandomValue);

# Task2
#  Subtask a:
RandomLaw1 = integer();
RandomLaw2 = integer();
i = 0;
repeat
{
	RandomLaw1[i] = sqrt(-2*log(runif(1))) * sin(2*pi*runif(1));
	RandomLaw2[i] = sqrt(-2*log(runif(1))) * cos(2*pi*runif(1));
	i = i + 1;
	if(i > 1000)
	{
		break;
	}
}
dev.new();
hist(RandomLaw1);
lillie.test(RandomLaw1);
ad.test(RandomLaw1);
pearson.test(RandomLaw1);
cvm.test(RandomLaw1);

dev.new();
hist(RandomLaw2);
lillie.test(RandomLaw2);
ad.test(RandomLaw2);
pearson.test(RandomLaw2);
cvm.test(RandomLaw2);

#  Subtask b:
RandomLawBN = integer();
i = 0;
j = 0;
#change N to check different results and compare with task 1;
N = 48;
temp = 0;
for (i in 0:999) {
    for (j in 1:N) {
        temp = temp + runif(1) - 0.5;
    }
    RandomLawBN[i] = temp * sqrt(12 / N);
    temp = 0;
}
dev.new();
hist(RandomLawBN);
lillie.test(RandomLawBN);
ad.test(RandomLawBN);
pearson.test(RandomLawBN);
cvm.test(RandomLawBN);

# Task 3
sgma = function(i_val) {
    return (1 / ((pi + i ^ 2) ^ 2));
}
N = 8000;
#  Subtask a:
lambda_a = function(i_val) {
    return(i_val * pi);
}
a_func = function(t_val) {
    result = c();
    for (i in 1:N) {
        result[i] = cos(lambda_a(i) * t_val) * runif(1) + sin(lambda_a(i) * t_val) * runif(1);
    }
    return(sum(result));
}
get_realization = function(){
    result = c();
    time_value = 0;
    for (i in 1:101) {
        result[i] = a_func(time_value);
        time_value = time_value + 0.01;
    }
    return(result);
}

dev.new();
realizations = list();
t_vals = seq(0, 1, by = 0.01);
for (i in 1:10) {
    realizations[[i]] = get_realization();
    plot(t_vals, realizations[[i]], xlim = range(t_vals), ylim = range(realizations[[i]]), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * i);
    lines(t_vals, realizations[[i]], xlim = range(t_vals), ylim = range(realizations[[i]]), pch = 16, col = 10 * i);
    par(new = TRUE);
}

# Task 3
#  Subtask b
lambda_b = function(i_val) {
    return(runif(1, i_val * pi(), (i_val + 1) * pi()));
}
b_func = function(t_val) {
    result = c();
    for (i in 1:N) {
        result[i] = cos(lambda_b(i) * t_val) * runif(1) + sin(lambda_b(i) * t_val) * runif(1);
    }
    return(sum(result));
}

get_realization_b = function() {
    result = c();
    time_value = 0;
    for (i in 1:101) {
        result[i] = b_func(time_value);
        time_value = time_value + 0.01;
    }
    return(result);
}

dev.new();
realizations_b = list();
for (i in 1:10) {
    realizations_b[[i]] = get_realization_b();
    plot(t_vals, realizations_b[[i]], xlim = range(t_vals), ylim = range(realizations_b[[i]]), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * i);
    lines(t_vals, realizations_b[[i]], xlim = range(t_vals), ylim = range(realizations_b[[i]]), pch = 16, col = 10 * i);
    par(new = TRUE);
}

