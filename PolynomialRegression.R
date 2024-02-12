# a program that performs polynomial regression 

source("GaussianGaussJordan.R")

# function that checks if the number of data is greater than the degree
CheckDegree <- function(degree, variables){
  if (degree >= length(variables[[1]])){ # if the degree is greater than, returns FALSE
    return (FALSE); 
  }
  return (TRUE);
}

# function that computes for the values of n+1 x n+1
Summation <- function(degree, variables){
  xVar <- variables[[1]] # stores the x variables [independent]
  summVec <- c() # empty vector for the summation values
  
  for (n in 0:(2*degree)){ # up to 2n exponent
    summation <- 0 # starting value for summation
    for (x in xVar){ # traverse in the x variables
      summation <- summation + (x ^ n) # summations
    }
    summVec <- c(summVec, summation) # appends to the empty vector
  }
  return (summVec) # return value
}

# function that computes for the n+1+1 column
RHS <- function(degree, variables){
  xVar <- variables[[1]] # stores the x variables [independent]
  yVar <- variables[[2]] # stores the y variables [dependent]
  RHS <- c() # empty vector for the rhs values
  
  for (n in 0:degree){ # up to n exponent
    summation <- 0 # starting value for summation
    for (i in 1:length(xVar)){ # traverse up to length of variables
      summation <- summation + (xVar[i]^n * yVar[i]) # computes for the rhs
    }
    RHS <- c(RHS, summation) # appends to the empty vector
  }
  return (RHS) # return value
}

# creates an augmented coefficient matrix
MakeMatrix <- function(degree, variables){
  summations <- Summation(degree, variables) # summations
  rhs <- RHS(degree, variables) # rhs
  
  mat <- matrix(c(0), nrow = (degree+1), ncol = (degree+2)) # makes a placeholder matrix
  
  for (i in 1:(degree+1)){ # replaces values of matrix per column
      mat[,i] <- summations[i:(degree+i)] 
  }
  mat[, (degree+2)] <- rhs # replaces the rhs
  row.names(mat) <- 1:nrow(mat)
  colnames(mat) <- 1: ncol(mat)
  
  return (mat) # return value
}

# function that creates the polynomial equation in string
PolynomialString <- function(degree, coeff){
  stringVec <- c() # empty vector
  for (i in 1:(degree+1)){ # traverse through the coefficients and pastes " * x ^ "
    stringVec <- c(stringVec, paste(coeff[i], i-1, sep = " * x ^ "))
  }
  stringVec <- paste(stringVec, collapse = " + ") # pastes " + " between values as separator
  polyString <- paste("function (x) ", stringVec) # pastes at the beginning
  
  return (polyString) # return value
}

# main function for computing the Polynomial Regression
PolynomialRegression <- function(degree, variables, estimate){
  if (CheckDegree(degree, variables) == FALSE){ 
    return(NULL) # if not applicable to solve
  }else{
    mat <- MakeMatrix(degree, variables) # stores the augcoeffmatrix
    coeff <- GaussJordanMethod(list(augcoeffmatrix = mat))$solution # stores the coefficients
    string <- PolynomialString(degree, coeff) # stores the polynomial string
    func <- eval(parse(text = string)) # stores the polynomial function
    
    fx <- func(estimate)

    list <- list(augcoeffmatrix = mat, coefficients = coeff, polynomial_string = string, 
                 polynomial_function = func, value = fx) # makes into a labelled list
    return (list) # return value
  }
}
