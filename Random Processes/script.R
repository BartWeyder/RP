m = 30;
k = 6;
n = 21;
lambda = 4;
single_realization= 0;
realizations = list();
step_functions = list();
max_realization_values = c();
intervals = c();
average_k_time = 0;
for (i in 1:11) {
    temp_intervals = 0;
    for (j in 1:m) {
        temp_intervals[j] = rpois(n = 1, lambda);
        single_realization[j + 1] = single_realization[j] + temp_intervals[j];
        if (j == k) {
            average_k_time[i] = temp_intervals[j];
        }
    }
    intervals[i] = mean(temp_intervals);
    realizations[[i]] = single_realization;
    max_realization_values[i] = max(single_realization);
    step_functions[[i]] = stepfun(y = seq(0, m + 1), x = single_realization);
    single_realization= 0;
}

max_realization_value = max(max_realization_values);

for (i in 1:10) {
    plot.stepfun(x = step_functions[[i]], do.points = TRUE, col.points = i * 10, col.hor = i * 10, verticals = FALSE,
             lwd = 3, xlab = "Time", ylab = "", main = "", xlim = c(0, max_realization_value));
    par(new = TRUE);
}
plot.stepfun(x = step_functions[[11]], do.points = TRUE, col.points = 110, col.hor = 110, verticals = FALSE,
             lwd = 3, xlab = "Time", ylab = "", main = "", xlim = c(0, max_realization_value));
title(main = "Poisson process realizations", cex.main = 1.9);

# Task3
#  Subtask 1:
dev.new();
hist(intervals);


#  Subtask 2:
dev.new();
hist(average_k_time);


#  Subtask 3:
dev.new();
temp_val_for_hist = c();
for (i in 1:11) {
    temp_val_for_hist[i] = realizations[[i]][n];
}
hist(temp_val_for_hist);
