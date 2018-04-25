source("ONEDIM.R")

for (i in 1:4) {
    S = get_S();
    if (i == 1) {
        new_range = dikhotom(sven());
    }
    if (i == 2) {
        new_range = golden_cut(sven());
    }
    if (i == 3) {
        new_range = dsk_paulo(sven());
    }
    if (i == 4) {
        new_range = dsk_paulo(sven());
        S = x_values[[3]] - x_values[[1]];
    }

    lambda_ranges[[i]] = new_range;
    lambda = runif(1, new_range[1], new_range[2]);
    lambda_values[i] = lambda;
    current_x = current_x + lambda * S;
    x_values[[i]] = current_x;
    f_values[i] = f(current_x);

    iteration = iteration + 1;
}