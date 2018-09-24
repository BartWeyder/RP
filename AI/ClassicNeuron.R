ClassicNeuron <- setRefClass(
  "ClassicNeuron",
  fields = list
  (
    startWeight = "vector",
    x0 = "vector",
    y_expected = "numeric",
    max_fault = "numeric",
    x = "vector",
    y = "vector",
    fault = "vector",
    delta = "vector",
    delta_w = "list",
    w = "list"
  ),
  methods = list(
    learn = function() {
      # first iter
      i <- 1
      x <<- append(x, sum(x0 * startWeight))
      y <<- append(y, 1 / (1 + exp(-x[i])))
      fault <<-
        append(fault, abs((y_expected - y[i]) / y_expected))
      if (fault[i] <= max_fault) {
        print(c("Optimal weight: ", startWeight, "on iteration#", i))
      } else {
        delta <<- append(delta, y[i] * (1 - y[i]) * (y_expected - y[i]))
        delta_w <<- append(delta_w, list(delta[i] * x0))
        w <<- append(w, list(startWeight + delta_w[[i]]))
        
        while (TRUE) {
          i = i + 1
          x <<- append(x, sum(x0 * w[[i - 1]]))
          y <<- append(y, 1 / (1 + exp(-x[i])))
          fault <<-
            append(fault, abs((y_expected - y[i]) / y_expected))
          if (fault[i] <= max_fault) {
            print(c("Optimal weight: ", w[[i - 1]], "on iteration#", i))
            break
          } else {
            delta <<- append(delta, y[i] * (1 - y[i]) * (y_expected - y[i]))
            delta_w <<-
              append(delta_w, list(delta[i] * x0))
            w <<-
              append(w, list(w[[i - 1]] + delta_w[[i]]))
          }
        }
      }
    },
    get_optimal_weight = function() {
      return(tail(w))
    },
    import_from_file = function(path) {
      input_data <- read.csv(path)
      startWeight <<- input_data$w
      x0 <<- input_data$x
      y_expected <<- input_data$y[1]
      max_fault <<- input_data$max_fault[1]
    },
    export_to_file = function(path = "export.csv") {
      write.table(tail(w, n = 1)[[1]], file = path, row.names = FALSE, col.names = c("w"))
    }
  )
)

# or
a <- ClassicNeuron()
a$import_from_file("input.csv")
# a <- ClassicNeuron(
#   startWeight = c(0.001, 0.001, 0.002),
#   x0 = c(1, 1, 1),
#   y_expected = 0.5,
#   max_fault = 0.001
# )
a$learn()
a$export_to_file("output.csv")
