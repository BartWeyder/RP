library(ggplot2)

NG = 52
n = NG / 8
n = round(n, 0)
N = 2 ^ n
M = 8

s1 = c()

i = 1:N
s1 = 2 * runif(1) + NG * cos(2 * M * pi * i / N) * (1 + 0.1 * runif(1)) +
  17 * cos(4 * M * pi * i / N + runif(1)) +
  3 * cos(7 * M * pi * i / N) * (runif(1) + NG)

M1 = N
is_gauss = TRUE

g1 = function(t)
{
  if (is_gauss)
    return(exp(-(t ^ 2) / 2))
  else
  {
    if (0 <= t && t <= 1 / 2)
      return(1)
    else if (1 / 2 <= t && t <= 1)
      return(-1)
    else
      return(0)
  }
}

f2 = function(j, k, x)
{
  return(2 ^ (j / 2) * g1((2 ^ j) * x - k))
}

W = function(l, j)
{
  temp = c()
  for(i in 0:(N-1))
  {
    temp[i + 1] = s1[i + 1] * f2(l, j, i)
    
    # print(temp[i])
  }
  return(sum(temp))
}

pr = function(i, l)
{
  temp = c()
  for (j in 0:M1)
  {
    temp[j + 1] = W(l,j) * f2(l, j, i) / (2 ^ (2 * l))
  }
  return(sum(temp))
}

d = function(i)
{
  temp = c()
  for(l in 0:N)
  {
    temp[l + 1] = pr(i,l)
  }
  return(sum(temp))
}

d_ = c()
for (i in 0:(N-1))
{
  d_[i + 1] = d(i)
}

ggplot() + geom_line(aes(x = 1:length(d_), y = d_)) + geom_line(aes(x = 1:length(s1), y = s1), col = "red")

ggplot() + geom_line(aes(x = 1:length(d_), y = d_/2)) + geom_line(aes(x = 1:length(s1), y = s1), col = "red")




l = 1:M
j = 1:M1
z = matrix(nrow = length(l), ncol = length(j))
for(i in l)
{
  for(k in j)
  {
    z[i,k] = W(i,k)
  }
}

rgl = persp3d(l, j, z, col = "grey", alpha=0.7)
