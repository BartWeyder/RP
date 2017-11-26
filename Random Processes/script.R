n = 30
lambda1 = 4
lambda2 = 0.6
s1 = 0
s2 = 0
s = list();
for (i in 1:n) {
    s1[i + 1] = s1[i] + rpois(n = 1, lambda1);
    print(s1[i]);
    s2[i + 1] = s2[i] + rpois(n = 1, lambda1);
}
p1 = stepfun(y = seq(0, n + 1), x = s1);
p2 = stepfun(y = seq(0, n + 1), x = s2);
plot.stepfun(x = p1, do.points = TRUE, col.points = "blue", col.hor = "blue", verticals = FALSE,
             lwd = 3, xlab = "Zeit", ylab = "", main = "", xlim = c(0, max(max(s1), max(s2))));
plot.stepfun(x = p2, do.points = TRUE, col.points = "red", col.hor = "red", verticals = FALSE,
             lwd = 3, add = TRUE, xlim = c(0, max(max(s1), max(s2))));
#title(main = "Poisson process realizations", cex.main = 1.9)