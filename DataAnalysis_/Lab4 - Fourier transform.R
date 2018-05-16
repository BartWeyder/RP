library(ggplot2);
NG = 52;
M = 8;
n = NG / 4;
N = 2 ^ n;

i = c(1:N)
S2 = 2 * runif(1) + NG * cos(2*M*pi*i/N)*(1 + 0.1*runif(1)) +
  17 * cos(4 * M * pi * i / N + runif(1)) + 3 * cos(5*M*pi*i/N) * 
  (runif(1) + NG);

# va = 0.42 - 0.5 * cos(2 * pi * i / N) + 0.08 * sin(4 * pi * i / N);
# S1 = S2 * va;

vb = 0.54 - 0.46 * cos(2 * pi * i / N);
S1 = S2 * vb;

# S1 = S2;

right_bound = (N/2) - 1;
l = 1:((N/2) - 1);
i_ = i - 1;
A = l;
for(j in l) {
  A[j] = 2 / N * sum(S1 * cos(2*pi*i_*j/N));
}
A0 = sum(S1) / N;
AN_2 = sum(S1 * cos(pi * i_)) / N;
A = c(A0, A);
A = c(A, AN_2);

B = c(0:(N/2));
for(j in 0:(N/2)) {
  B[j+1] = 2 / N * sum(S1 * sin(2 * pi * i_ * j / N));
}

C = sqrt(A^2 + B^2);
first_plot = ggplot() +
  geom_point(aes(y=C[1:30], x = c(0:29))) +
  geom_line(aes(y=C[1:30], x = c(0:29)));

# -----
dl = c(0:(N/2));
sum_1 = 0;
sum_2 = 0;

for(k in i_)
{
  sum_1 = 0;
  sum_2 = 0;
  for(j in 0:(N/2)) 
  {
    sum_1 = sum_1 + A[j+1]*cos(2*pi*j*k/N);
    sum_2 = sum_2 + B[j+1]*sin(2*pi*j*k/N);
  }
  
  dl[k+1] = sum_1 + sum_2;
}
dl = head(dl, -1);

ggplot() + geom_line(aes(y = dl, x = 1:length(dl)), col = "blue") + 
  geom_line(aes(y = dl_1, x = 1:length(dl_1)), col = "red") +
  geom_line(aes(y = dl_2, x = 1:length(dl_2)), col = "green")

dl_fast = fft(S1)
