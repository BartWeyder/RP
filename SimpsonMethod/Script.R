isB <- FALSE
isF <- FALSE

source("Input.R", encoding = "Windows-1252")
source("SimpsonMethod.R", encoding = "Windows-1252")

while (TRUE) {
  if (isB && isF) {
    switch(menu(
      c("Input Boundaries", "Input Function", "Calculate", "Exit")
    ),
    {
      if (!input_boundaries())
        isB <<- TRUE
    },
    {
      print("To cancel input type 'cancel'")
      while (TRUE) {
        res <- input_function()
        if (!res) {
          isF <<- TRUE
          break
        } else if (res == 4) {
          break
        }
      }
    },
    {
      res <- simpson_method(f, a, b, n)
      if (is.na(res))
        print("Stack overflow, too high values")
      else
        print(paste("Calculated integral:", res))
    },
    {
      break
    })
    
  }
  else {
    if (!isB)
      print("Enter boundaries to start calculation")
    if (!isF)
      print("Enter Function to start calculation")
    switch(menu(c("Input Boundaries", "Input Function", "Exit")), {
      if (!input_boundaries())
        isB <<- TRUE
    }, {
      print("To cancel input type 'cancel'")
      while (TRUE) {
        res <- input_function()
        if (!res) {
          isF <<- TRUE
          break
        } else if (res == 4) {
          break
        }
      }
    }, {
      break
    })
  }
  
}
