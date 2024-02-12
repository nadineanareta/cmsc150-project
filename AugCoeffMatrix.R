# a program that makes an augmented coefficient matrix

# gets the variable names
GetColumn <- function(system){
  return (names(formals(system[[1]]))) # outputs each variable name in string
  # names() and formals() from GeeksforGeeks
}

# appends all function in string
StrFunction <- function(system){ 
  str_func <- c() # empty vector
  for (i in 1:length(system)){
    str_func <- c(str_func, strsplit(deparse(system[[i]], width.cutoff = 500), "\\+")[[1]])
    # appends all "function (x1, x2,..., xn)" in a single vector
  }
  return (str_func) # return value
}

# compare if same number of the unknown variables, then return
CheckMatrix <- function(system){
  str_func <- StrFunction(system) # "function (x1, x2,..., xn)"
  for (k in 1:(length(str_func)-1)){ # traverse through the vector
    if (!identical(str_func[k], str_func[k+1])){ # checks if identical
      return (FALSE)
    }
  } 
  return (TRUE)
}

# gets the coefficients and constants of the system of linear equations
GetCoeff <- function(system){
  # an empty vector to store all the coefficients and constants
  vec <- c()
  # traverse through all the equations
  for (i in 1:length(system)){
    # an empty vector to store the equation in string
    # splits and deparse the string to access invidual variables
    str_val <- c()
    str_val <- c(str_val, strsplit(deparse(system[[i]], width.cutoff = 500), "\\+"))[[2]]
    str_val <- strsplit(str_val, "\\*")
    
    # an empty vector to store the constant
    constant <- 0
    # traverse through the str_val and 
    # checks if the length is 1 or if it is constant
    for (term in str_val){
      if (length(term) == 1){
        # stores the constant to the vector
        constant <- as.numeric(term) * -1
      }
    }
    
    # takes out the white spaces in the values
    for (j in 1:length(str_val)){
      str_val[[j]] <- gsub(" ", "", str_val[[j]])
    }
    
    # initialization for columns, length, and vector for coefficients
    column <- GetColumn(system)
    n <- length(column)
    coeff <- numeric(n)
    
    # nested loop to check if the variables is identical
    # to the column then stores the coefficient to the vector
    for (i in 1:length(coeff)){
      for (j in 1:length(str_val)){
        if (length(str_val[[j]] == 2)){
          if (identical(str_val[[j]][2], column[i])){
            coeff[i] <- as.numeric(str_val[[j]][1])
          }
        }
      }
    }
    # appends both coefficient and constant of that equation to another vector
    # returns the values of all the system of equation
    vec <- c(vec, coeff, constant)
  }
  return (vec)
}

# function that makes and returns the augmented coefficient matrix
AugCoeffMatrix <- function(system){
  valid <- CheckMatrix(system)
  if (valid != TRUE){
    print("Matrix is not valid.")
    cat("\n")
    return (NULL)
  }
  # initialization of coefficients, number of rows and column
  coeff <- GetCoeff(system)
  row <- length(system)
  column <- GetColumn(system)
  matrix <- matrix(coeff, nrow = row, ncol = length(column)+1, 
                   TRUE, dimnames = list(1:row, c(column, "B")))
  list <- list(variables = column, augcoeffmatrix = matrix)
  return (list)
}
