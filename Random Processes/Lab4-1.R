# Lab 4 Part 1

p0 = c(0.2, 0.2, 0.2, 0.2, 0.2);

P_original = matrix(
    c(0.5, 0.2, 0.3, 0, 0,
        0.2, 0.6, 0.2, 0, 0,
        0, 0, 1, 0, 0,
        0.2, 0.4, 0, 0.3, 0.1,
        0, 1, 0, 0, 0),
    ncol = 5,
    nrow = 5
);
P_original = t(P_original);

P_block = matrix(
    c(
    1, 0, 0, 0, 0,
    0, 1, 0, 0, 0,
    0.3, 0.2, 0.5, 0, 0,
    0, 0.4, 0.2, 0.3, 0.1,
    0.2, 0.6, 0.2, 0, 0
    ),
    ncol = 5,
    nrow = 5
);
P_block = t(P_block);

unit_matrix = matrix(0, ncol = 3, nrow = 3);
for (i in 1:3) {
    unit_matrix[i, i] = 1;
}

absorb_realization = list();

get_row = function(arg) {
    rand = runif(1);
    new_arg = 0;
    for (i in 1:5) {
        new_arg = new_arg + arg[i];
        if (rand <= new_arg)
            return(i);
        }
}

for (i in 1:6) {
    realization = c();
    realization[1] = get_row(p0);
    for (j in 2:10) {
        realization[j] = get_row(P_block[realization[j - 1], ]);
    }
    absorb_realization[[i]] = realization;
}

nodes = seq(1, 5, 1);
for (i in 1:6) {
    plot(seq(1, 10, 1), absorb_realization[[i]], xlim = range(seq(1, 10, 1)), ylim = c(1, 5), xlab = "Step", ylab = "Status (node)",
     main = "Absorbation realization (6 pieces)", pch = 16, col = 10 * i);
    dev.new();
}

# Task 2
Q = matrix(nrow = 3, ncol = 3);
for (i in 1:3) {
    for (j in 1:3) {
        Q[i, j] = P_block[i + 2, j + 2];
    }
}
#fundamental 
N = solve(unit_matrix - Q);

#tau
average_time_tau = N %*% c(1, 1, 1);

R = matrix(nrow = 3, ncol = 2);
for (i in 1:3) {
    for (j in 1:2) {
        R[i, j] = P_block[i + 2, j];
    }
}

#
absorb_possibility = N %*% R;

#
average_absorb_time = c(0.2, 0.2, 0.2) %*% average_time_tau;


#Task 3
realizations_amount = 2000;
vals_table = matrix(0, nrow = realizations_amount, 4);

check_absorb = function(arg, iter) {
    if (arg == 1 || arg == 2) {
        return(TRUE);
    }
    return(FALSE);
}

average_time_in_node = c(0, 0, 0);
for (i in 1:realizations_amount) {
    realization = c();
    is_absorbed = FALSE;
    realization[1] = get_row(p0);
    if (check_absorb(realization[1])) {
        vals_table[i, 4] = 1;
        is_absorbed = TRUE;
    }
    else {
        vals_table[i, realization[1] - 2] = vals_table[i, realization[1] - 2] + 1;
    }

    counter = 2;

    while (!is_absorbed) {
        realization[counter] = get_row(P_block[realization[counter - 1],]);
        if (check_absorb(realization[counter])) {
            vals_table[i, 4] = counter;
            is_absorbed = TRUE;
        }
        else {
            vals_table[i, realization[counter] - 2] = vals_table[i, realization[counter] - 2] + 1;
        }
        counter = counter + 1;
    }
}

for (i in 1:4) {
    print(mean(vals_table[, i]));
}