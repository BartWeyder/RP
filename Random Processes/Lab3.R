p1 = c(0.1, 0.2, 0.3, 0.4);
p2 = c(0.2, 0.3, 0, 0.5);
p3 = c(0.1, 0.1, 0.1, 0.7);
p4 = c(0.5, 0.4, 0.1, 0);
p = array(c(p1, p2, p3, p4), dim = c(4, 4));
p0 = c(0.25, 0.25, 0.25, 0.25);
get_row = function(arg) {
    rand = runif(1);
    #print(rand);
    new_arg = 0;
    for (i in 1:4) {
        new_arg = new_arg + arg[i];
        if (rand <= new_arg)
            return (i);
    }
    
    
    
}

result = c(0, 0, 0, 0);
for (i in 1:10000) {
    print(paste("Begin of ", i, " iteration"));
    cur = get_row(p0);
    print(paste("0: ", cur));
    for (j in 1:4) {
        cur = get_row(p[, cur]);
        print(paste(j, ": ", cur));
    }
    
    for (j in 1:4) {
        if (cur == j) {
            result[j] = result[j] + 1;
        }
    }
    print(result);
    print(paste("End of ", i, " iteration"));
}

print((p %*% p %*% p %*% p) %*% p0);


