#Lab 1


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

#Integral 3 ***ASK***

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
