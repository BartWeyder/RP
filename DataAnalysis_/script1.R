x = c(2, 4, 1, 7, 3, 11, 14, 15, 21, 4)
y = c(7, 6, 4, 11, 2, 21, 31, 23, 40, 15)

regr = lm(y ~ (x ^ 3))
ggplot() + geom_point(aes(x = x, y = y)) + geom_line(aes(x = x, y = regr$fitted.values))
summary(regr)

x = c(1, 3, 4, 5)
p = c(0.2, 0.3, 0.4, 0.1)
plot(x, p)
lines(x, p)
sp = c(0.2)
for (i in 2:4) {
    sp[i] = sp[i-1] + p[i]
