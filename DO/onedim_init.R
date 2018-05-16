library(rlist);
library(dplyr);

source("gift.R");

f = function(x, lambda = 0, S = 0)
{
  x = x + lambda * S;
  return((x[1] - 8) ^ 2 - x[1] * x[2] + 3 * (x[2]) ^ 2 + 4 * (x[3]) ^ 2);
}
