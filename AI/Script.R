# generated values
x = 0.1658833
y_expected = 0.913274
# possible fault value
max_fault = 0.05
# generating first weight
w0 = runif(1, 0, 0.001)
#iterator 
i = 1
# calc functions
get_x = function(weight) { return(x * weight) }
get_y = function(x_val) { return(1 / (1 + exp(-x_val))) }

#making first iteration
X = c(get_x(w0))
y = c(get_y(X[i]))
fault = c(abs((y_expected - y[i]) / y_expected))
if (fault[i] <= max_fault) {
    print(c("Optimal weight: ", w0, "on iteration#", i))
} else {
    neuron_fault = c(y[i] * (1 - y[i]) * (y_expected - y[i]))
    correcting_coef = c(neuron_fault[i] * x)
    w = c(w0 + correcting_coef)

    #launching loop
    while (TRUE) {
        i = i + 1
        X[i] = get_x(w[i - 1])
        y[i] = get_y(X[i])
        fault[i] = abs((y_expected - y[i]) / y_expected)
        if (fault[i] <= max_fault) {
            print(c("Optimal weight: ", w[i - 1], "on iteration#", i))
            break
        } else {
            neuron_fault[i] = y[i] * (1 - y[i]) * (y_expected - y[i])
            correcting_coef[i] = neuron_fault[i] * x
            w[i] = w[i - 1] + correcting_coef[i]
        }
    }
}