# Task 3
sgma = function(i_val) {
    return(1 / ((pi + i ^ 2) ^ 2));
}
N = 8000;
#  Subtask a:
lambda_a = function(i_val) {
    return(i_val * pi);
}
a_func = function(t_val) {
    result = c();
    for (i in 1:N) {
        result[i] = cos(lambda_a(i) * t_val) * rnorm(1)*sgma(i) + sin(lambda_a(i) * t_val) * rnorm(1)*sgma(i);
    }
    return(sum(result));
}
get_realization = function() {
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
for (i in 1:9) {
    realizations[[i]] = get_realization();
    plot(t_vals, realizations[[i]], xlim = range(t_vals), ylim = range(realizations[[i]]), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * i);
    lines(t_vals, realizations[[i]], xlim = range(t_vals), ylim = range(realizations[[i]]), pch = 16, col = 10 * i);
    par(new = TRUE);
}
realizations[[10]] = get_realization();
plot(t_vals, realizations[[10]], xlim = range(t_vals), ylim = range(realizations[[10]]), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * 10);
lines(t_vals, realizations[[10]], xlim = range(t_vals), ylim = range(realizations[[10]]), pch = 16, col = 10 * 10);

# Task 3
#  Subtask b
lambda_b = function(i_val) {
    return(rnorm(1, i_val * pi, (i_val + 1) * pi));
}
b_func = function(t_val) {
    result = c();
    for (i in 1:N) {
        result[i] = cos(lambda_b(i) * t_val) * rnorm(1)*sgma(i) + sin(lambda_b(i) * t_val) * rnorm(1)*sgma(i);
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

par(new = FALSE);

dev.new();
dev.next();
realizations_b = list();
for (i in 1:9) {
    realizations_b[[i]] = get_realization_b();
    plot(t_vals, realizations_b[[i]], xlim = range(t_vals), ylim = c(-10, 10), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * i);
    lines(t_vals, realizations_b[[i]], xlim = range(t_vals), ylim = c(-10, 10), pch = 16, col = 10 * i);
    par(new = TRUE);
}
realizations_b[[10]] = get_realization_b();
plot(t_vals, realizations_b[[10]], xlim = range(t_vals), ylim = c(-10, 10), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10 * 10);
lines(t_vals, realizations_b[[10]], xlim = range(t_vals), ylim = c(-10, 10), pch = 16, col = 10 * 10);

par(new = FALSE);

mid_val = list();
mid_value = c();
mid_val_b = list();
mid_value_b = c();
for (i in 1:101) {
    mid_val[[i]] = NaN * seq(10);
    mid_val_b[[i]] = NaN * seq(10);
    for (j in 1:10) {
        mid_val[[i]][j] = realizations[[j]][i];
        mid_val_b[[i]][j] = realizations_b[[j]][i];
    }
    mid_value[i] = mean(mid_val[[i]]);
    mid_value_b[i] = mean(mid_val_b[[i]]);
}

dev.new();
plot(t_vals, mid_value, xlim = range(t_vals), c(-10, 10), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 10);
lines(t_vals, mid_value, xlim = range(t_vals), ylim = c(-10, 10), pch = 16, col = 10);
dev.new();
plot(t_vals, mid_value_b, xlim = range(t_vals), c(-10, 10), xlab = "t", ylab = "value",
     main = "noise-less data", pch = 16, col = 30);
lines(t_vals, mid_value_b, xlim = range(t_vals), ylim = c(-10, 10), pch = 16, col = 30);
