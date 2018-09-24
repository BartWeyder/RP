DoubleLayerPerseptrone <- setRefClass(
  "DoubleLayerPerseptrone",
  fields = list(
    # input values
    x = "numeric",
    y_expected = "numeric",
    max_fault = "numeric",
    w12_ = "numeric",
    w23_ = "numeric",
    # calculated values
    w12 = "vector",
    w23 = "vector",
    y2 = "vector",
    x3 = "vector",
    y3 = "vector",
    fault = "vector",
    delta_3 = "vector",
    delta_w23 = "vector",
    delta_2 = "vector",
    delta_w12 = "vector"
  ),
  methods = list(
    import_from_file = function(path) {
      input_data <- read.csv(path)
      x <<- input_data$x
      y_expected <<- input_data$y_expected
      max_fault <<- input_data$max_fault
      w12_ <<- input_data$w12_
      w23_ <<- input_data$w23_
    },
    export_to_file = function(path = "output.csv") {
      write.table(c(tail(w12, n = 1), tail(w23, n = 1)), 
                  file = path, row.names = FALSE, col.names = c("Optimal weights"))
    },
    learn = function() {
      i <- 1
      y2 <<- append(y2, 1 / (1 + exp(-x * w12_)))
      x3 <<- append(x3, y2[i] * w23_)
      y3 <<- append(y3, 1 / (1 + exp(-x3[i])))
      fault <<-
        append(fault, abs((y_expected - y3[i]) / y_expected))
      if (fault[i] <= max_fault) {
        print(c("Optimal weight12: ", w12_, c("Weight23:"), w23_, "on iteration#", i - 1))
        return()
      }
      
      # correcting part
      delta_3 <<- append(delta_3, y3[i] * (1 - y3[i]) * (y_expected - y3[i]))
      delta_w23 <<- append(delta_w23, y2[i] * delta_3[i])
      w23 <<- append(w23, w23_ + delta_w23[i])
      
      #correcting hidden neuron                         
      delta_2 <<- append(delta_2, y2[i] * (1 - y2[i]) * (delta_3[i] * w23_))
      delta_w12 <<- append(delta_w12, x * delta_2[i])
      w12 <<- append(w12, w12_ + delta_w12[i])
      i <-  i + 1
      
      while (TRUE) {
        y2 <<- append(y2, 1 / (1 + exp(-x * w12[i - 1])))
        x3 <<- append(x3, y2[i] * w23[i - 1])
        y3 <<- append(y3, 1 / (1 + exp(-x3[i])))
        fault <<-
          append(fault, abs((y_expected - y3[i]) / y_expected))
        if (fault[i] <= max_fault) {
          print(c("Optimal weight12: ", w12[i - 1], c("Weight23:"), w23[i - 1], "on iteration#", i - 1))
          break
        }
        # correcting part
        delta_3 <<- append(delta_3, y3[i] * (1 - y3[i]) * (y_expected - y3[i]))
        delta_w23 <<- append(delta_w23, y2[i] * delta_3[i])
        w23 <<- append(w23, w23[i - 1] + delta_w23[i])
        
        #correcting hidden neuron                         
        delta_2 <<- append(delta_2, y2[i] * (1 - y2[i]) * (delta_3[i] * w23[i - 1]))
        delta_w12 <<- append(delta_w12, x * delta_2[i])
        w12 <<- append(w12, w12[i - 1] + delta_w12[i])
        i <-  i + 1
      }
    }
  )
)

#a <- DoubleLayerPerseptrone(x = 2.3, y_expected = 0.2559, max_fault = 0.001, w12_ = 0.002, w23_ = 0.001)
a <-  DoubleLayerPerseptrone()
a$import_from_file("2lpersep_input.csv")
a$learn()
a$export_to_file("2lpersep_output.csv")