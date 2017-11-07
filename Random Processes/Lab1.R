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
dev.new();
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
