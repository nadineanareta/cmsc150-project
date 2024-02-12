# a program that performs a diet solver using a simplex method minimization

options(max.print = 10000)

# setting up of the matrix
# its parameters is the matrix formed from the csv file of all food and nutrients
# and input or the index of food in the matrix
SetUpMat <- function(database, inputs){
  if (length(inputs) == 0) return (NULL)
  # creates a placeholder matrix of value 0
  # the number of row is 22 [nutrients*2] + number of inputs + 1 for the objective function
  # the number of columns is the length of input + 1 for the solution column
  mat <- matrix(0, nrow=22+length(inputs)+1, ncol = length(inputs)+1,
                dimnames = list(1:(22+length(inputs)+1), 1:(length(inputs)+1)))
  
  # vector of minimum and maximum value of each nutrients
  minimum <- c(2000, 0, 0, 0, 0, 25, 50, 5000, 50, 800, 10)
  maximum <- c(-2250, -300, -65, -2400, -300, -100, -100, -50000, -20000, -1600, -30)
  constraints <- c(minimum, maximum)
  
  # a vector that stores the nutrients of chosen food
  nutrients <- c()
  for (i in inputs){
    nutrients <- c(nutrients, as.numeric(database[i,3:13]))
  }
  
  # a vector that stores the price per serving of each chosen food
  # this would be the objective function
  objective <- c()
  for (i in inputs){
    objective <- c(objective, as.numeric(database[i, 2]))
  }
  # appends a vlaue of 1 to the objective function
  objective <- c(objective, 1)
  
  # replaces per column
  # the first 11 are the constraints for minimization
  # the second 11 are the constraints for maximization
  # the following rows and columns would be constraints for 0-10 serving
  for (i in 1:length(inputs)){
    mat[1:11, i] <- nutrients[(11*i-10):(11*i)]
    mat[12:22, i] <- -1 * nutrients[(11*i-10):(11*i)]
    mat[22+i, i] <- -1
    mat[22+i, ncol(mat)] <- -10
  }
  
  # replaces the last row to the objective function
  # replaces teh first 22 rows, last column with the nutrients values
  mat[nrow(mat), ] <- objective
  mat[1:22, ncol(mat)] <- constraints
  
  return(mat)
}

# transposes the matrix
TransposeMat <- function(database, inputs){
  if (is.null(SetUpMat(database, inputs))) return (NULL)
  return (t(SetUpMat(database, inputs)))
}

# makes the iniial tableau needed for simplex method
InitialTableau <- function(database, inputs){
  # stores the matrix
  mat <- TransposeMat(database, inputs)
  
  if (is.null(mat)) return (NULL)
  # slack variables
  slack <- (nrow(mat)-1)
  cols <- ncol(mat) + slack + 1
  
  # makes a placeholder matrix with a value of 0
  # the number of rows would be the rows of transposed matrix
  # the number of columns is ncol of transposed matrix + slack + 1 for the solution
  cmat <- matrix(0, nrow = nrow(mat), ncol = cols,
                 dimnames = list(1:nrow(mat), 1:cols))
  
  # empty vector for column names
  columnName <- c()
  ctr1 <- 1
  for (i in 1:ncol(cmat)){
    if (i < ncol(mat)){ # for the variables
      columnName <- c(columnName, paste("S", i, sep=""))
    }else if (i >= ncol(mat) && i <= (ncol(cmat)-2)){ # for the slack variables
      columnName <- c(columnName, paste("x", ctr1, sep=""))
      ctr1 <- ctr1 + 1
    }else if (i == ncol(cmat)){ # solution column
      columnName <- c(columnName, "Solution")
    }else{ # objective function column
      columnName <- c(columnName, "Z")
    }
  }
  
  # declares the column names
  colnames(cmat) <- columnName
  
  # replaces with 1 that is needed for slack variables
  ctr <- 0
  for (i in 1:(cols-1)){
    if (i < (cols-slack-1)){
      mat[nrow(mat), i] <- -1 * mat[nrow(mat), i]
      cmat[,i] <- mat[,i]      
    }else{
      ctr <- ctr + 1
      cmat[ctr, i] <- 1
    }
  }
  
  # replaces last column with the solution values
  # the value of last row, last column is 0
  cmat[,ncol(cmat)] <- mat[,ncol(mat)]
  cmat[ctr, ncol(cmat)] <- 0
  
  return (cmat)
}

# checks matrix if all values of bottom row are negative
# returns true if there is still negative values
CheckMat <- function(mat){
  for (i in 1:(ncol(mat)-2)){
    if (mat[nrow(mat), i] < 0){
      return (TRUE)
    }
  }
  return (FALSE)
}

# appends onto a list
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

# performs the simplex method 
SimplexMethod <- function(database, inputs){
  # stores the matrix
  # stores the boolean value for iteration
  mat <- InitialTableau(database, inputs)
  if (is.null(mat)) return (NULL)
  valid <- CheckMat(mat)
  
  # stores matrix and basic solution every iteration
  matIteration <- list()
  basicSolution <- list()
  iterations <- 0
  
  # iterates while there is still negative values at teh bottom row
  while (valid){
    # gets the pivot column
    PivotColumn <- which.min(mat[nrow(mat),1:(ncol(mat)-2)])
    
    #comparison value
    # iteration to solve test ratio and gets index of pivot row
    ans <- 100
    for (i in 1:nrow(mat)){
      testratio <- mat[i, ncol(mat)]/mat[i, PivotColumn]
      if (testratio < ans && testratio > 0){
        ans <- testratio
        PivotRow <- i
      }
    }
    
    # declares the pivot element and normalized row
    PivotElement <- mat[PivotRow, PivotColumn]
    
    if (PivotElement <= 0){
      return (NULL)
    }
    NormalizedRow <- mat[PivotRow,]/PivotElement
    mat[PivotRow,] <- NormalizedRow
    
    # performs elimination
    for (j in 1:nrow(mat)){
      if (PivotRow == j){
        next
      }else{ # computation for identity matrix
        NormalizedRow <- mat[j, PivotColumn] * mat[PivotRow,]
        mat[j,] <- mat[j,] - NormalizedRow
      }
    }
    
    # checks if there is still negative values at the bottom row
    # increments the iteration of simplex method
    valid <- CheckMat(mat)
    iterations <- iterations + 1
    
    # if the iteration is valid, appends the basic solution and matrix
    # onto a list
    if (valid){
      basicSolution <- lappend(basicSolution, BasicSolution(mat))
      matIteration <- lappend(matIteration, mat)
    }else{
      break
    }
  }
  # appends the final tableau 
  # appends all necessary information onto a labelled list
  matIteration <- lappend(matIteration, mat)
  list <- list(MATRICES_PER_ITERATION = matIteration, BASIC_SOLUTIONS = basicSolution, ITERATIONS = iterations)
  return (list)
}

# function to get teh basic solution
BasicSolution <- function(mat){
  # an empty vector
  bsol <- c()
  
  # nested loop to get each basic solution
  for (j in 1:(ncol(mat)-1)){
    for (i in 1:nrow(mat)){
      if (mat[i, j] == 1){
        bsol <- c(bsol, mat[i, ncol(mat)])
      }
    }
  }
  return (bsol)
}

# function that gets teh final solution
FinalSolution <- function(database, inputs){
  list <- SimplexMethod(database, inputs)
  
  if (is.null(list)) return (NULL)
  # gets the final tableau
  mat <- list$MATRICES_PER_ITERATION[[list$ITERATIONS]]
  
  # gets the final solution
  # the bottom row for the slack variables
  # the value at the last row and last column for the objective function
  fsolution <- mat[nrow(mat), (ncol(mat)-nrow(mat)):(ncol(mat)-2)]
  fsolution <- c(fsolution, mat[nrow(mat), ncol(mat)])
  
  # appends to a list
  list <- lappend(list, FINAL_SOLUTION = fsolution)
  
  return (list)
}

# a printing statement if there are no inputs
NoInput <- function(){
  mat <- matrix(c("Total Cost", ">>>",0), nrow = 1, ncol = 3)
  rownames(mat) <- 1
  colnames(mat) <- c("Food", "Serving", "Cost")
  
  return (mat)
}

# function that makes the matrix/table
TableVal <- function(database,inputs){
  if (is.null(FinalSolution(database, inputs))) return (NULL)
  # initializations and storing of variables
  fsolution <- FinalSolution(database,inputs)$FINAL_SOLUTION
  final_sol <- numeric()
  food_name <- character()
  cost <- numeric()
  serving <- character()
  
  # iterates over the fsolution
  for (i in 1:(length(fsolution)-1)){
    # disregards all 0's in the final solution
    if (fsolution[i] != 0){
      food_name <- append(food_name, database[inputs[i], 1])
      serving <- append(serving, fsolution[i])
      cost <- append(cost, fsolution[i] * as.numeric(database[inputs[i],2]))
      final_sol <- append(final_sol, fsolution[i])
    }
  }

  
  # appends the values of the bottom row
  food_name <- append(food_name, "Total Cost")
  serving <- append(serving, ">>>")
  cost <- append(cost, fsolution[length(fsolution)])
  final_sol <- append(final_sol, fsolution[length(fsolution)])
  
  # makes the matrix and returns
  mat <- matrix(c(food_name, serving, cost), nrow = length(cost), ncol = 3, byrow = FALSE)
  colnames(mat) <- c("Food", "Serving", "Cost")
  rownames(mat) <- c(1:nrow(mat))
  
  list <- list(matrix = mat, fsol = final_sol)

  return(list)
}