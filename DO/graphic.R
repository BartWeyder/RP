require(rgl)
x_ = y_ = seq(-3, 3, .25)
graphic_func = function(x, y)
{
  return((1 - x) ^ 2 + 100 * (y - x ^ 2) ^ 2)
}
z = outer(x_, y_, graphic_func)
persp3d(x_, y_, z, col="gray", alpha=0.5)
x_lines = unlist(lapply(x, "[[", 1))
y_lines = unlist(lapply(x, "[[", 2))
z_lines = graphic_func(x_lines, y_lines)
lines3d(x_lines, y_lines, z_lines, col="royalblue4")
points3d(
  head(x_lines, n = 13),
  head(y_lines, n = 13),
  head(z_lines, n = 13),
  col = "green",
  size = 6
)
points3d(tail(x_lines, n = 2), tail(y_lines, n = 2), tail(z_lines, n = 2), col =
           "purple4", size = 6)