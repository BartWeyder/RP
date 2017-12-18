P = t(matrix(
    c(
      0.5, 0.2, 0.3, 0, 0,
      0.2, 0.6, 0.2, 0, 0,
      0.3, 0.2, 0.3, 0.1, 0.1,
      0.2, 0.4, 0, 0.3, 0.1,
      0.3, 0.1, 0.1, 0.3, 0.2
      ),
      nrow = 5, ncol = 5
));
p0 = c(0.1, 0.3, 0.2, 0.3, 0.1);

get_row = function(arg) {
    rand = runif(1);
    new_arg = 0;
    for (i in 1:5) {
        new_arg = new_arg + arg[i];
        if (rand <= new_arg)
            return(i);
    }
}



#Task 5
realizations_amount = 1000;
steps_amount = 10;
first_time = matrix(0, nrow = realizations_amount, ncol = 5);
average_in_condition = matrix(0, nrow = realizations_amount, ncol = 5);

current_condition = get_row(p0);
first_time[1, current_condition] = 1;
average_in_condition[1, current_condition] = 1;
prev_condition = current_condition;

for (i in 1:realizations_amount) {
    for (j in 2:steps_amount) {
        current_condition = get_row(P[prev_condition,]);
        if (first_time[i, current_condition] == 0)
            first_time[i, current_condition] = j;
        average_in_condition[i, current_condition] = average_in_condition[i, current_condition] + 1;
    }
}
