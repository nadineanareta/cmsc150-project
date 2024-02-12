# a program that performs quadratic spline interpolation

source("GaussianGaussJordan.R")

# a function that stores the string function()
StrFunc <- function(variables, interval){
  str <- c() # an empty vector
  for (i in 1:interval){ # traverses through the number of interval
    if (i == 1){ # does not pastes a1; ignores the a1 since it is not needed [a1 = 0]
      str <- c(str, paste("b", i, ", c", i, sep = ""))
    }else{
      str <- c(str, paste("a", i, ", b", i, ", c", i, sep = ""))
    }
  }
  str <- paste(str, collapse = ", ")
  str <- paste("function (", str, ") ", sep = "") # concatenates the string to function()
  return (str)
}

# solves for the first condition
Condition1 <- function(variables, interval){
  # stores the variables for easy access
  # initialization of an empty vector
  # that stores the result of condition 1
  x <- variables[[1]]
  fx <- variables[[2]]
  str <- StrFunc(variables, interval)
  condStr <- c()
  # condition 1 traverses through 2-n
  # in which n is the interval
  for (i in 2:interval){
    condStr <- c(condStr, paste(str, (x[i])^2, " * a", (i-1), " + ", x[i], " * b", (i-1), " + 1 * c", (i-1), " + ", -1*fx[i], sep = ""))
    condStr <- c(condStr, paste(str, (x[i])^2, " * a", (i), " + ", x[i], " * b", (i), " + 1 * c", (i), " + ", -1*fx[i], sep = ""))
  }

  # returns the vector that stores the result of condition in string
  return (condStr)
}

# a function that uses the second condition
Condition2 <- function(variables, interval){
  # initialization of variables and empty vector
  x <- variables[[1]]
  fx <- variables[[2]]
  i <- interval + 1
  str <- StrFunc(variables, interval)
  condStr <- c()
  
  # string manipulation for the second condition
  condStr <- c(condStr, paste(str, (x[1])^2, " * a1 + ", x[1], " * b1 + 1 * c1 + ", -1*fx[1], sep = ""))
  condStr <- c(condStr, paste(str, (x[i])^2, " * a", interval," + ", x[i], " * b", interval, " + 1 * c", interval, " + ", -1*fx[i], sep = ""))
  
  # return value
  return (condStr)
}

# function that uses the third condition
Condition3 <- function(variables, interval){
  # stores the variables
  # initialize an empty vector
  x <- variables[[1]]
  str <- StrFunc(variables, interval)
  condStr <- c()
  
  # condition 3 traverse through 2-n, intervals
  for (i in 2:interval){
    condStr <- c(condStr, paste(str, 2*x[i], " * a", (i-1), " + 1 * b", (i-1)," + ", -1*2*x[i]," * a", (i), " + -1 * b", (i), sep = ""))
  }
  
  # returns the result from condition 3 in string
  return (condStr)
}

# function that makes the functions
MakeFunctions <- function(variables, interval){
  # accesses all results from the three conditions
  # condition 4 is not present since it is just a1 = 0
  
  cond1 <- Condition1(variables, interval)
  cond2 <- Condition2(variables, interval)
  cond3 <- Condition3(variables, interval)
  
  # initialization of empty vectors
  # appends all condition in the vector
  list1 <- c()
  system <- c()
  list1 <- c(list1, cond1, cond2, cond3)
  
  # performs eval() and parse() on all the string functions
  for (i in 1:length(list1)){
    system <- c(system, eval(parse(text = list1[i])))
  }
  
  # calls or makes the augmented matrix
  # stores it in a labelled list and returns the list
  result <- AugCoeffMatrix(system)
  str <- append("a1", result$variables)
  answer <- c(0, GaussJordanMethod(result)$solution)

  return(answer)
}

# a function for quadratic spline interpolation
QuadraticSplineInterpolation <- function(variables, interval, estimate){
  # stores the coefficients
  x <- variables[[1]]
  y <- variables[[2]]
  
  mat <- matrix(c(x, y), ncol = 2, byrow = FALSE)
  
  sorted <- mat[order(mat[,1]),]
  values <- MakeFunctions(list(x = sorted[,1], y = sorted[,2]), interval)

  
  # checks if the values are NULL or does not exist
  # returns NILL
  if (is.null(values)){
    return (NULL)
  }else{
    # an empty vector
    # initializations
    str_func <- c()
    i <- 1
    
    # pastes or concatenates the values/coefficients to a function
    while (i <= length(values)){
      str_func <- c(str_func, paste("function (x) ", values[i], " * x^2 + ",
                                    values[i+1], " * x + ", values[i+2], sep = ""))
      i <- i + 3
    }
    
    func <- c()
    func_used <- c()
    
    # traverses through the string
    # evaluates and parses the functions
    for (i in 1:length(str_func)){
      func <- c(func, eval(parse(text = str_func[i])))
    }
    
    # traverses through x and compares where the estimate number falls
    # if found, it estimates with the correct function
    for (i in 2:length(sorted[,1])){
      if (estimate <= sorted[i,1]){
        answer <- func[[i-1]](estimate)
        func_used <- func[[i-1]]
        break
      }else{
        next
      }
  }
    
    # puts all the function per interval, the function used, and estimated value in a list
    # returns the list
    list <- list(Function_Per_Interval = func, Function = func_used, Estimated_Value = answer)
    return (list)
  }
}

# plots the quadratic spline
PlotQuadraticSpline <- function(func, xlim, col, lwd = 2){
  curve(func, xlim = xlim, col = col, lwd = lwd,  add = TRUE)
}
