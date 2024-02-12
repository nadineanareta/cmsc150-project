# a program that implements the Gaussian and Gauss-Jordan Elimination Method

source("AugCoeffMatrix.R")

# swap function
SwapRow <- function(PivotRow, a, i){
  temp <- a[PivotRow,] # temporarily holds the pivot row
  a[PivotRow,] <- a[i,] # swap
  a[i,] <- temp
  
  return (a) # returns the updated matrix
}

# function that creates the upper triangular matrix needed for gaussian method
UpperTriangular <- function(result){
  a <- result$augcoeffmatrix # gets the augcoeffmatrix using the AugCoeffMatrix function in exer 3
  n <- nrow(a) # gets the number of rows

  for (i in 1:(n-1)){ # n-1 iterations
    PivotRow <- which.max(abs(a[i:n, i])) + (i-1) # gets the index of the maximum absolute value in the matrix

    if (a[PivotRow, i] == 0){ # terminates if equal to zero
      print("STOP")
      break
    }
    a <- SwapRow(PivotRow, a, i) # performs swap method
    for (j in (i+1):n){ # iteration below the pivot row
      PivotElement <- a[i,i] # gets the pivot element
      Multiplier <- a[j,i]/PivotElement # gets he multiplier
      NormalizedRow <- Multiplier * a[i,] # gets the normalized row
      a[j,] <- a[j,] - NormalizedRow # updates the current eval row
    }
  }
  return (a) # returns the upper triangular matrix
}

# gets the solution set for gaussian method
BackwardSubstitution <- function(result){
  a <- UpperTriangular(result) # the upper triangular matrix
  n <- nrow(a) # length of rows
  x <- numeric(n) # vector for solutions
  rhs <- a[,ncol(a)] # access the rhs
  
  for (i in n:1){
    x[i] <- rhs[i] / a[i,i] # gets the last solution value
    if (i > 1){ # if not the first row
      rhs[1:(i - 1)] <- rhs[1:(i - 1)] - a[1:(i - 1), i] * x[i] # computation in getting the solution set
    }  
  }
  return (x)
}

# function for gaussian method
GaussianMethod <- function(result){
  variables <- result$variables # gets the variable
  augcoeffmatrix <- UpperTriangular(result) # gets the aug coeff matrix of the system
  solution <- BackwardSubstitution(result) # gets the solution set
  if (length(solution) == 0 || is.infinite(length(solution))){ # if the solution is zero or infinite number
    return (NA) # returns NA
  }else{
    list <- list(variables = variables, augcoeffmatrix = augcoeffmatrix, solution = solution) # appends to a labelled list
    return(list) # return value
  }
}

# function that creates the identity matrix for gauss-jordan method
IdentityMatrix <- function(result){
  a <- result$augcoeffmatrix # gets the augcoeffmatrix
  n <- nrow(a) # gets the number of rows

  for (i in 1:n){
    if (i != n){
      PivotRow <- which.max(abs(a[i:n, i])) + (i-1) # gets the pivot row

      if (a[PivotRow, i] == 0){ # if the pivot row is 0
        print("STOP")
        break
      }
      a <- SwapRow(PivotRow, a, i) # swap function
    }
    a[i,] <- a[i,] / a[i,i] # divides the row with pivot element
    for (j in 1:n){
      if (i == j){
        next
      }else{ # computation for identity matrix
        NormalizedRow <- a[j,i] * a[i,]
        a[j,] <- a[j,] - NormalizedRow
      }
    }
  }
  return (a) # return value
}

# function for gauss-jordan
GaussJordanMethod <- function(result){
  variables <- result$variables # gets the variables
  augcoeffmatrix <- IdentityMatrix(result) # gets the identity matrix
  solution <- as.vector(augcoeffmatrix[,ncol(augcoeffmatrix)]) # gets the solution
  if (length(solution) == 0 || is.infinite(length(solution))){ # if the solution is zero or infinite numbers
    return (NA) # return value
  }else{
    list <- list(variables = variables, augcoeffmatrix = augcoeffmatrix, solution = solution) # appends to a labelled list
    return(list) # return value
  }
}
