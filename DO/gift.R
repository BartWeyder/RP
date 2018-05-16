sven <- function(x0 , s0, delta_param = 0.1) 
{
  # x0 should be vector
  # s0 should be vector
  # delta_param - numeric
  x <- list(x0 = x0)
  lambd <- c(0)
  delta <- 0
  delta <-
    delta_param * norm(x$x0, type = "2") / norm(s0, type = "2")
  
  f1 <- f(x$x0, lambd[1] - delta, s0)
  f2 <- f(x$x0, lambd[1], s0)
  f3 <- f(x$x0, lambd[1] + delta, s0)
  f_values <- c(f2)
  
  if (f1 >= f2 && f2 >= f3)
  {
    delta <- delta
    lambd[2] <- lambd[1] + delta
    f_values <- c(f_values, f3)
  } 
  else if (f1 <= f2 && f2 <= f3) 
  {
    delta <- -1 * delta
    lambd[2] <- lambd[1] + delta
    f_values <- c(f_values, f1)
  } 
  else if (f1 >= f2 && f2 <= f3) 
  {
    a <- lambd[1] - delta
    b <- lambd[1] + delta
    f_values <- c(f_values, f1, f3)
    return(c(a, b))
  } 
  else
    return("function is not unimodal")
  
  x <- list.append(x, x$x0 + lambd[2] * s0)
  
  i <- 2
  while (f_values[i] <= f_values[i - 1]) 
  {
    lambd[i + 1] <- lambd[i] + delta * (2 ^ (i - 1))
    x <- list.append(x, x$x0 + lambd[i + 1] * s0)
    f_values <- c(f_values, f(x$x0, lambd[i + 1], s0))
    i <- i + 1
  }
  
  lambd[i + 1] <- lambd[i - 1] + (lambd[i] - lambd[i - 1]) / 2
  x <- list.append(x, x$x0 + lambd[i + 1] * s0)
  f_values <- c(f_values, f(x$x0, lambd[i + 1], s0))
  
  lambda_and_its_f <<- data.frame(lambda = lambd, f_value = f_values)
  lambda_and_its_f <<- arrange(lambda_and_its_f, lambda)
  
  m <- lambda_and_its_f %>%
    mutate(index = rownames(lambda_and_its_f)) %>%
    filter(f_value == min(f_value)) %>%
    select(index)
  
  m <- as.numeric(m[1, 1])
  a <- lambda_and_its_f$lambda[m - 1]
  b <- lambda_and_its_f$lambda[m + 1]
  return(c(a, b))
}

golden_section <- function(x0, s0, interval, epsilon = 0.001) 
{
  x <- list(x0 = x0)
  interval[1] -> a
  interval[2] -> b
  
  gold <- 0.6180339887
  
  L <- b - a
  x1 <- a + (1 - gold) * L
  x2 <- a + gold * L
  
  f1 <- f(x$x0, x1, s0)
  f2 <- f(x$x0, x2, s0)
  
  while (L >= epsilon) 
  {
    if (f1 <= f2) {
      a <- a
      b <- x2
      L <- b - a
      x2 <- x1
      f2 <- f1
      x1 <- a + (1 - gold) * L
      f1 <- f(x$x0, x1, s0)
    } else if (f1 >= f2) {
      a <- x1
      b <- b
      L <- b - a
      x1 <- x2
      f1 <- f2
      x2 <- a + gold * L
      f2 <- f(x$x0, x2, s0)
    }
  }
  return((a + b) / 2)
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

dsk_powell <- function(x0, s0, interval, epsilon = 0.001) {
  x <- list(x0 = x0)
  a <- interval[1]
  b <- interval[2]
  
  x1 <- a
  x2 <- (a + b) / 2
  x3 <- b
  
  f1 <- f(x$x0, x1, s0)
  f2 <- f(x$x0, x2, s0)
  f3 <- f(x$x0, x3, s0)
  #calls_count1 <- calls_count1 + 3
  
  x_star <- x2 + (abs(x2 - x1) * (f1 - f3) / (2 * (f1 - 2 * f2 + f3)))
  f_star <- f(x$x0, x_star, s0)
  #calls_count1 <- calls_count1 +1
  
  x_and_f <<-
    data.frame(x_value = c(x1, x2, x_star, x3),
               f_value = c(f1, f2, f_star, f3))
  x_and_f <<- arrange(x_and_f, x_value)
  x_and_f <<- mutate(x_and_f, index = rownames(x_and_f))
  
  x_min <- x2
  f_min <- x_and_f %>%
    filter(x_value == x2) %>%
    select(f_value)
  
  f_min <- as.numeric(f_min[1, 1])
  
  f_star <- x_and_f %>%
    filter(x_value == x_star) %>%
    select(f_value)
  
  f_star <- as.numeric(f_star[1, 1])
  
  while ((abs(x_min - x_star) >= epsilon) &&
         (abs(f_min - f_star) >= epsilon)) 
  {
    f_min <- min(x_and_f$f_value)
    
    m <- x_and_f %>%
      #mutate(index = rowname(x_and_f)) %>%
      filter(f_value == f_min) %>%
      select(index)
    
    m <- as.numeric(m[1, 1])
    
    x1 <- x_and_f$x_value[m - 1]
    x2 <- x_and_f$x_value[m]
    x3 <- x_and_f$x_value[m + 1]
    
    x_min <- x2
    f1 <- x_and_f$f_value[m - 1]
    f2 <- f_min#x_and_f$f_value[m]
    f3 <- x_and_f$f_value[m + 1]
    
    if (abs(x3 - x2) == abs(x2 - x1)) 
    {
      x_star <- x2 + (abs(x2 - x1) * (f3 - f1)) / (2 * (f1 - 2 * f2 + f3))
    } 
    else
    {
      a1 <- (f2 - f1) / (x2 - x1)
      a2 <- (1 / (x3 - x2)) * ((f3 - f1) / (x3 - x1) - (f2 - f1) / (x2 -
                                                                      x1))
      x_star <- ((x1 + x2) / 2 - (a1 / (2 * a2)))
    }
    if (abs(x_min - x_star) >= epsilon) 
    {
      f_star <- f(x$x0, x_star, s0)
      #calls_count1 <- calls_count1 + 1
    } 
    else
    {
      f_star <- f2
    }
    x_and_f <<-
      data.frame(x_value = c(x1, x2, x_star, x3),
                 f_value = c(f1, f2, f_star, f3))
    x_and_f <<- arrange(x_and_f, x_value)
    x_and_f <<- mutate(x_and_f, index = rownames(x_and_f))
  }
  return(x_star)
}

onedim_search <-
  function(x0,
           s0,
           epsilon = 0.001,
           delta_param = 0.1,
           type) 
  {
    return(type(x0, s0, sven(x0, s0, delta_param = 0.1), epsilon = 0.001))
  }