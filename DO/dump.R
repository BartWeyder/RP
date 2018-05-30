golden_cut = function(x, s, range, epsilon = 0.001) {
  L = range[2] - range[1];
  x1 = range[1] + 0.382 * L;
  x2 = range[2] + 0.618 * L;
  f1 = f(x, x1, s)
  f2 = f(x, x2, s)
  while (L > epsilon) {
    if (f1 <= f2) {
      range[2] = x2;
      L = range[2] - range[1];
      x2 = x1;
      f2 = f1
      x1 = range[1] + 0.382 * L;
      f1 = f(x, x1, s)
    }
    else if (f1 >= f2) {
      range[1] = x1;
      L = range[2] - range[1];
      x1 = x2;
      f1 = f2
      x2 = range[1] + 0.618 * L;
      f2 = f(x, x2, s)
    }
  }
  return((range[1] + range[2]) / 2);
}

dichotomi <- function(x0, s0, interval, epsilon = 0.001) {
  x <- list(x0 = x0)
  a <- interval[1]
  b <- interval[2]
  #epsilon <- 0.001
  
  xm <- (b + a) / 2
  fm <- f(x$x0, xm, s0)
  L <- b - a
  
  while (L >= epsilon) 
  {
    x1 <- a + L / 4
    x2 <- b - L / 4
    f1 <- f(x$x0, x1, s0)
    f2 <- f(x$x0, x2, s0)
    
    if (f1 <= fm) 
    {
      b <- xm
      xm <- x1
      fm <- f1
    } 
    else if (f1 >= fm) 
    {
      if (f2 <= fm) 
      {
        a <- xm
        xm <- x2
        fm <- f2
      } 
      else if (f2 >= fm) 
      {
        a <- x1
        b <- x2
      }
    }
    L <- b - a
  }
  return((a + b) / 2)
}