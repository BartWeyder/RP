library(ggplot2)
n <- 1000 
delta <- 0.95
N <- 14
NG <- 52
x <- c()
y <- c()
R_d <- 4
t <- 1.645
F_delta <- 1.03

for(i in 1:n){
  x[i] <- i + runif(1,0,1)*N/NG
  y[i] <- N*runif(1,0,1)*x[i]+NG*runif(1,0,1)
}
linear_model <- lm(y ~ x)
coef <- linear_model$coefficients


xy <- as.data.frame(x)
xy$y <- y

ggplot(data = xy , mapping = aes(x = x,y = y)) +
  geom_point(col = "grey") +
  geom_line(aes(x,coef[1]+x*coef[2] ),col = "red")


y_ <- coef[1] + x*coef[2]

e <- y-y_

si_2 <- sum(e^2)/(n-2)*(1 - 1/n - (x-mean(x))^2/sum((x-mean(x))^2))

R <- max(abs(e/sqrt(si_2)))

if(!(R > R_d)){
  print("no emissions")
}

# H0: beta == b

sx_2 <- sum((x-mean(x))^2)/(n-1)
  
s_2 <- sum((y-coef[1]-coef[2]*x)^2)/(n-2)

s_beta <- sqrt(s_2/(sx_2*(n-1)))

if (abs(coef[2]) > t*s_beta){
  print("coeficient b is significant ")
}else{
  print("coeficient b is not significant ")
}

#H0: alpha == a

s_alpha <- sqrt(s_2*(1/n+mean(x)^2/((n-1)*sx_2)))

if (abs(coef[1]) > t*s_alpha){
  print("coeficient a is significant ")
}else{
  print("coeficient a is not significant ")
}

#H0: model is adequate

sy_2 <- sum((y-mean(y))^2)/(n-1)

if(s_2/sy_2 < F_delta){
  print("the model is adequate")
}else{
  print("the model is not adequate")
}

############# polynomial

sqare_model <- lm(y ~ x+ I(x^2))
sq_coef <- sqare_model$coefficients

ggplot(data = xy , mapping = aes(x = x,y = y)) +
  geom_point(col = "grey") +
  geom_line(aes(x,sq_coef[1] + x*sq_coef[2] + sq_coef[3]*x^2 ),col = "yellow") +
  geom_line(aes(x,coef[1]+x*coef[2] ),col = "red")

s_sqr_2 <- sum((y - (sq_coef[1]+sq_coef[2]*x+sq_coef[3]*x^2))^2)/(n-2) 
s_2;s_sqr_2

    #cub
cub_model <- lm(y ~ x+ I(x^2)+ I(x^3))
cub_coef <- cub_model$coefficients

ggplot(data = xy , mapping = aes(x = x,y = y)) +
  geom_point(col = "grey") +
  geom_line(aes(x,cub_coef[1] + x*cub_coef[2] + cub_coef[3]*x^2 + cub_coef[4]*x^3 ),col = "blue")+ 
  geom_line(aes(x,sq_coef[1] + x*sq_coef[2] + sq_coef[3]*x^2 ),col = "yellow")
  
s_cub_2 <- sum((y - (cub_coef[1]+cub_coef[2]*x+cub_coef[3]*x^2)+ cub_coef[4]*x^3)^2)/(n-2)

s_2;s_cub_2

N <- 14
x <- c(1:10)
y <- c(N+4.2,N+6.1,N+7.9,N+10.2,N+12.1,N+13.8,N+16.2,N+18,N+20.2,N+21.5)

lm(log(y) ~ x)

ggplot(data = data.frame(x =x1,y =y1),aes(x = x1, y = y1)) +
  geom_point()

x1 <- runif(100)
y1 <- 5-3*x1


