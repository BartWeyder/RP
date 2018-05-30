# grad
gradient = function(f,
                    x0,
                    type = "right",
                    h)
{
  res = c()
  f_0 = f(x0)
  if (type == "left")
  {
    
    res[1] = (-f(c(x0[1] - h, x0[2])) + f_0) / h
    res[2] = (-f(c(x0[1], x0[2] - h)) + f_0) / h
  }
  else if (type == "right")
  {
    res[1] = (f(c(x0[1] + h, x0[2])) - f_0) / h
    res[2] = (f(c(x0[1], x0[2] + h)) - f_0) / h
  }
  else if (type == "center")
  {
    res[1] = (f(c(x0[1] + h, x0[2])) - f(c(x0[1] - h, x0[2]))) / (2 * h)
    res[2] = (f(c(x0[1], x0[2] + h)) - f(c(x0[1], x0[2] - h))) / (2 * h)
  }
  else
    print("Gradient type ERROR")
  return(res)
}