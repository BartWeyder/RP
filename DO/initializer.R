library(rlist)
library(dplyr)

source("lambdaHandler.R")
source("gradient.R")

x0 = c(-1.2, 0)

epsilon_ = 0.001
onedim_epsilon = 0.01
function_calls = 0
grad_step = 0.01

f = function(x, lambda = 0, S = 0)
{
  function_calls <<- function_calls + 1
  x = x + lambda * S
  # return((x[1] - 1) ^ 2 + (x[2] - 1) ^ 2)
  return((1 - x[1]) ^ 2 + 100 * (x[2] - x[1] ^ 2) ^ 2)
}

source("SearchAlgorithm.R")
source("graphic.R")