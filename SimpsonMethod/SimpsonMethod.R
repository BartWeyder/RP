simpson_method = function(f, a, b, n = 1000) {
    h = (b - a) / n
    s = (f(a) + f(b)) * 0.5
    for (i in 1:(n - 1)) {
        xk = a + h * i
        xk1 = a + h * (i - 1)
        s = s + f(xk) + 2 * f((xk1 + xk) / 2)
    }
    x = a + h * n
    x1 = a + h * (n - 1)
    s = s + 2 * f((x1 + x) / 2)
    return(s * h / 3.0)
}
