f = function(x) { }
body = ""
a = 10
b = 12
n = 100
clc <- function() cat(rep("\n", 50))

input_boundaries = function() {
    print("To cancel input type 'cancel'.")
    input_result <- 1
    while (input_result) {
        clc()
        print("Write a (left boundary): ")
        buff <- readline()
        if (buff == "cancel")
            return(4)
        result <- tryCatch({
            a <<- as.double(buff)
        }, warning <- function(w) {
            print("Warning: Incorrect input")
            print(w)
            input_result <- 1
        }, error = function(e) {
            print("Error: Incorrect input")
            print(e)
            input_result <- 2
        })
        if (is.numeric(a) && !is.na(a))
            input_result <- 0
    }

    input_result <- 1
    while (input_result) {
        clc()
        print(paste("a = ", a, " Write b (right boundary): ", sep = ""))
        buff <- readline()
        if (buff == "cancel")
            return(4)
        result = tryCatch({
            b <<- as.numeric(buff)
        }, warning = function(w) {
            print("Warning: Incorrect input")
            print(w)
            input_result <- 1
        }, error = function(e) {
            print("Error: Incorrect input")
            print(e)
            input_result <- 2
        })
        if (!is.na(b)) {
            if (b <= a) {
                print("Right boundary must be bigger than left")
                input_result <- 3
            }
            else {
                input_result <- 0
            }
        }
    }

    input_result <- 1
    while (input_result) {
        clc()
        print("Write n (number of parting): ")
        buff = readline()
        if (buff == "cancel")
            return(4)
        result = tryCatch({
            n <<- as.numeric(buff)
        }, warning = function(w) {
            print("Warning: Incorrect input")
            print(w)
            input <- 1
        }, error = function(e) {
            print("Error: Incorrect input")
            print(e)
            input_result <- 2
        })
        if (!is.na(n)) {
            if (n < 2)
                input_result <- 3
            else
                input_result <- 0
        }  
    }

    print(paste("Boundaries: [", a, ", ", b, "]. n = ", n, sep = ""))
    return(0)
}

input_function = function(body = "") {
    
    if (body == "") {
        print("Enter function:")
        body <- readline()
    }
    if (body == "cancel")
        return(4)
    result <- 0
    result <- tryCatch({
        eval(parse(text = paste('f <<- function(x) { return(', body, ')}', sep = '')))
    }, warning = function(w) {
        print(w)
        return(1)
    },
    error = function(e) {
        print(e)
        return(2)
    })
    if(is.numeric(result) && result)
      return(result)
    result <- tryCatch({
        buff <- f(a)
        return(0)
    }, warning = function(w) {
        print("This function may contain faults or does not exist in this boundaries")
        print(w)
        return(1)
    }, error = function(e) {
        print("Error!")
        print(e)
        return(2)
    })
    if(is.numeric(result) && result)
      return(result)
    return(0)
}
