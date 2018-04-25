# Variant 8
# KM-52 
# Yevhenii Kizim
x0 = c(-1.2 * 8 - 5, -1.2 * 8 - 5);
current_x = x0;
x_values = list();
epsilon = 0.001;
S_vectors = list(c(1, 0), c(0, 1));
iteration = 2;
get_S = function() {
    if (iteration %% 2 == 0) {
        return(c(0, 1));
    }
    else if (iteration == 5) {
        return(x_values[[3]] - x_values[[1]]);
    }
    else {
        return(c(1, 0));
    }
}
f = function(x) {
    return((x[1] - 8) ^ 2 - x[1] * x[2] + 3 * (x[2]) ^ 2);
}
fl = function(lambda) {
    cur_x = current_x + lambda * get_S();
    return(f(cur_x));
}
f_values = c();
lambda_values = c();
lambda_ranges = list();

get_delta = function() {
    return(0.1 * norm(current_x, type = "2") / norm(get_S(), type = "2"));
}

sven = function() {
    #preps
    lambda = c(0);
    f_main_val = f(current_x);
    f_vals = c(f_main_val);
    iter = 2;
    delta = get_delta();
    S = get_S();

    if (f(current_x + delta * S) > f_main_val) {
        delta = -1 * delta;
    }
    lambda[iter] = delta;
    f_vals[iter] = f(current_x + lambda[iter] * S);
    iter = iter + 1;
    
    while (TRUE) {
        lambda[iter] = (2 ^ iter) * delta;
        f_vals[iter] = f(current_x + lambda[iter] * S);
        if (f_vals[iter] >= f_vals[iter - 1]) {
            # >=2 so we can find middle of range between last 2 x
            if (iter >= 3) {
                # returning sorted data because we don't know which will be smaller, delta may be < 0
                return(sort(c(lambda[iter - 2], (lambda[iter - 1] + lambda[iter]) / 2)));
            }
            else {
                return(sort(c(lambda[iter - 1], lambda[iter])));
            }
        }
        iter = iter + 1;
    }
}

dikhotom = function(range) {
    xm = (range[2] + range[1]) / 2;
    L = (range[2] - range[1]);

    while (L > epsilon) {
        x1 = range[1] + L / 4;
        x2 = range[2] - L / 4;
        if (fl(x1) <= fl(xm)) {
            range[2] = xm;
            xm = x1;
        }
        else if (fl(x1) >= fl(xm)) {
            if (fl(x2) <= fl(xm)) {
                range[1] = xm;
                xm = x2;
            }
            else if (fl(x2) >= fl(xm)) {
                range[1] = x1;
                range[2] = x2;
            }
        }
        L = range[2] - range[1];
    }
    return(range);
}

golden_cut = function(range) {
    L = range[2] - range[1];
    x1 = range[1] + 0.382 * L;
    x2 = range[2] + 0.618 * L;
    while (L > epsilon) {
        if (fl(x1) <= fl(x2)) {
            range[2] = x2;
            L = range[2] - range[1];
            x2 = x1;
            x1 = range[1] + 0.382 * L;
        }
        else if (fl(x1) >= fl(x2)) {
            range[1] = x1;
            L = range[2] - range[1];
            x1 = x2;
            x2 = range[1] + 0.618 * L;
        }
    }
    return(range);
}

dsk_paulo = function(range) {
    delta = get_delta();
    x1 = range[1];
    x1_val = fl(x1);
    x3 = range[2];
    x3_val = fl(x3);
    x2 = (x1 + x3) / 2;
    x2_val = fl(x2);
    x_star = 0;
    x_star_val = 0;

    repeat {
        a1 = (x2_val - x1_val) / (x2 - x1);
        a2 = 1 / (x3 - x2) * ((x3_val - x1_val) / (x3 - x1) - (x2_val - x1_val) / (x2 - x1));
        x_star = (x1 + x2) / 2 - a1 / (2 * a2);
        x_star_val = fl(x_star);
        if (abs(x_star - x2) > epsilon && abs(x_star_val - x2_val) > epsilon) {
            break;
        }
        else {
            if (x_star > x2) {
                x1 = x2;
                x1_val = fl(x1);
                x2 = x_star;
                x2_val = fl(x2);
            }
            else {
                x3 = x2;
                x3_val = fl(x3);
                x2 = x_star;
                x2_val = fl(x2);
            }
        }
        print(x1);
        print(x1_val);
        print(x2);
        print(x2_val);
        print(x3);
        print(x3_val);

    }

    if (x_star > x2) {
        x1 = x2;
        x1_val = fl(x1);
        x2 = x_star;
        x2_val = fl(x2);
    }
    else {
        x3 = x2;
        x3_val = fl(x3);
        x2 = x_star;
        x2_val = fl(x2);
    }
    return(c(x2, x_star));
}

selector = 1;


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
