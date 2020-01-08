#clear the environment before executing code:
remove(list = ls())

############ GLOBAL VARIABLES ###############
storeCurrentMatrix <- NULL
cornercaseCacheStateSaver <- NULL


############## END OF GLOBAL VARIABLES #####################


#### FUNCTIONS#####

# FUNCTION 1:
# makeCacheMatrix: Creates an object that has setters and getters, as well as cache for storing the latest inverse of a matrix.
# Parameters: x: accepts a matrix data structure.
#functions: getMatrix, setMatrix, getInverseMatrix,setInverseMatrix

makeCacheMatrix <- function(x = matrix()) {
  # Variables inside the object
  
  inverseMatrix <-
    NULL #variable to store the inverse of the matrix
  
  # Getters and Setters for the Matrix variable of the object
  getMatrix <-
    function()
      x # getter to retrieve the passed in matrix
  setMatrix <- function(y = matrix())
  {
    inverseMatrix <<- NULL
    x <<- y
  }
  
  
  # Getters and Setters for the Inverse Matrix variable of the object
  getInverseMatrix <- function()
    inverseMatrix
  setInverseMatrix <- function(y = matrix())
  {
    inverseMatrix <<- y
  }
  
  list(
    getMatrix = getMatrix,
    setMatrix = setMatrix,
    getInverseMatrix = getInverseMatrix,
    setInverseMatrix = setInverseMatrix
  )
  
  
}

# FUNCTION 2:
# cacheSolve: Used to check if the object being supplied, already has an inverse matrix cached. If the object has no inverse, then a new inverse matix is created.
# Parameters: x: accepts makeCacheMatrix object

cacheSolve <- function(x = makeCacheMatrix()) {
  ##################################### START of HELPER FUNCTIONS #######################################3
  #Helper function 1: Gets the old Matrix
  getOldMatrix <-
    function()
      storeCurrentMatrix # getter to retrieve formerly processed matrix
  
  #Helper function 2: Stores a Cached copy, centrally, this is done to handle corner case scenarios.
  setCornerCaseStateSaverMatrix <-
    function(y = matrix())
      # setter to store matrix/formerly processed matrix
    {
      cornercaseCacheStateSaver <<- y
      
    }
  
  #Helper 3: Stores the current matrix who's inverse was calculated
  setMatrix <-
    function(y = matrix())
      # setter to store matrix/formerly processed matrix
    {
      storeCurrentMatrix <<- y
      
    }
  
  #Helper 4: This performs the matrix inversion as well as has checks to ensure that , singular matices get ignored
  processCache <- function(matrixtoprocess = matrix())
  {
    #Check if the matrix is invertible or singular
    if (det(matrixtoProcess) == 0)
    {
      print("The matrix is singular, and has no inverse")
      
    }
    else
    {
      print("1:Calculating the inverse of the matrix")
      x$setInverseMatrix(solve(matrixtoProcess))    # Solve and set the inverse of the matrix
      setCornerCaseStateSaverMatrix(x$getInverseMatrix()) #BackupCache for corner case scenario
      setMatrix(matrixtoProcess) # Store the current processed matrix who's inverse is calculated , as old MAtrix
      x$getInverseMatrix() # return the inverse matrix result
    }
  }
  
  #########  END of HELPER FUNCTIONS ########################################################3
  
  
  
  
  matrixtoProcess <- x$getMatrix()   # Set the matrix to ptocess
  oldmatrixProcessed <- getOldMatrix() # Get old matrix
  
  
  #If there is no old matrix that was processed in the past
  # perform the inverse calculation
  if (is.null(oldmatrixProcessed))
  {
    processCache(matrixtoProcess)
    
  }
  #If old matrix was already present, check if the new matrix and old matrix are the same, if not
  # recalculate a new inverse
  else
    #Step1: check if rows and coulmns are same or different
  {
    if (nrow(matrixtoProcess) != nrow(oldmatrixProcessed))
    {
      print("The matrix has changed")
      print("Calculating the inverse of the matrix")
      processCache(matrixtoProcess)
    }
    else if (ncol(matrixtoProcess) != ncol(oldmatrixProcessed))
    {
      print("The matrix has changed")
      print("Calculating the inverse of the matrix")
      processCache(matrixtoProcess)
    }
    else
      #Step2: Check if contents of the both matrices are the same
    {
      matdiff <- sum(as.integer(matrixtoProcess == oldmatrixProcessed))
      if (matdiff == nrow(matrixtoProcess) * ncol(matrixtoProcess))
      {
        print("The matrices are equal; retrieving inverse from the Cache")
        if (is.null(x$getInverseMatrix()))
          #Corner case: intializing the same object twice in a row, with same content.
        {
          #print("Corner case: retiriving from the Central Cache: which is good Caching design")
          #print(cornercaseCacheStateSaver)
          x$setInverseMatrix(cornercaseCacheStateSaver)
        }
        else
          #Step3: As long as the dimensions and the contents of both matrices match, fetch from the cache
        {
          #print("Normal Caching scenario as per assignment - retrieveing from object as assignmern requires: which is bad Caching design")
          #print(x$getInverseMatrix())
        }
        
      }
      else
      {
        print("The matrix has changed")
        print("Calculating the inverse of the matrix")
        processCache(matrixtoProcess)
      }
    }
    
    
  }
  #Return a matrix that is the inverse of 'x'
  x$getInverseMatrix()
}




#Test Bench for the Caching code:

#Scenario 1: Initializing an object for the very first time
mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)


#Scenario 2: Passing in the same object to exercise caching capability
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

#Scenario 3: Passing in a new matrix vector to check, if Caching logic recognizes the change

mat2 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = FALSE)
resultmatrix <- makeCacheMatrix(mat2)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

#Scenario 4: changing the dimensions and values of the matrix, to check if Caching logic recognizes the change
# Also passing in a singular matrix, to check if the code can handle such a case
mat2 <- matrix(2:17, 4, 4)
resultmatrix2 <- makeCacheMatrix(mat2)
cacheVar <- cacheSolve(resultmatrix2)

#Scenario 5: Re-initiazing and passing in the same object again and again, to check if the Cache is optimized to handle
# such a scenario

mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

#scenario 6: Passing in the same matrix via different objects to see , if caching still optimizes that scenario
mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

mat2 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat2)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

mat3 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat3)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

#Scenario 7: using the setMatrix to insert a new matrix in the place of the old one
mat4 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat4)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

mat5 <- matrix(c(1, 0, 1, 3, 4, 1), 2, 2, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat5)
cacheVar <- cacheSolve(resultmatrix)
print(cacheVar)

#More basic test benches from the Coursera forum.
# Matrices for testing the R functions
# makeCacheMatrix and cacheSolve
# in the Coursera R Programming Course
#
# First,
# If you haven't read Leonard Greski's invaluable
# [TIPS] Demystifying makeVector()  Post
# be sure to do so.
#
# A simple matrix m1 with a simple matrix inverse n1
# Define
m1 <- matrix(c(1 / 2, -1 / 4, -1, 3 / 4), nrow = 2, ncol = 2)
m1

# m1
#[,1]  [,2]
#[1,]  0.50 -1.00
#[2,] -0.25  0.75

# You can use m1 to test your
# makeCacheMatrix and cacheSolve functions.
# Since the grading is done on the correctness of your
# makeCacheMatrix and cacheSolve functions and your
# comments on how they work, using this (or some other)
# test matrix to check your code
# before submitting it
# is OK relative to the Coursera Honor Code.
# (Checking code with test cases is always a good idea.)
#
# m1 was constructed (using very simple linear algebra, so
# no references are given, almost surely the examples
# in this post have been given many times before)
# to have a simple marrix inverse, call it n1.
# This means  m1 %*% n1 (%*% is matrix multiply in R)
# is the 2 row by 2 column Identity matrix I2
I2 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
I2
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1

# And so (by linear algebra) n1 %*% m1 is also equal I2.
# (If n1 is the inverse of m1 then m1 is the inverse of n1.)
# With m1 defined as above, n1 ( the inverse of m1) is
n1 <- matrix(c(6, 2, 8, 4), nrow = 2, ncol = 2)
n1
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4

# Checks:
m1 %*% n1
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1

n1 %*% m1
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1

solve(m1)
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4

solve(n1)
#[,1]  [,2]
#[1,]  0.50 -1.00
#[2,] -0.25  0.75

# So if you have programmed your functions
# correctly (in the file cachematrix.R),
# (that, and your comments-explanation of how they work
# are what you are graded on)
# and sourced cachematrix.R so they are
# available in your R session workspace, then doing
#
myMatrix_object <- makeCacheMatrix(m1)

# and then
# cacheSolve(myMatrix_object)

# should return exactly the matrix n1
cacheSolve(myMatrix_object)
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4

# calling cacheSolve again should retrieve (not recalculate)
# n1
cacheSolve(myMatrix_object)
#getting cached data
#[,1] [,2]
#[1,]    6    8
#[2,]    2    4

# you can use the set function to "put in" a new matrix.
# For example n2
n2 <- matrix(c(5 / 8, -1 / 8, -7 / 8, 3 / 8), nrow = 2, ncol = 2)
myMatrix_object$setMatrix(n2)
# and obtain its matrix inverse by
cacheSolve(myMatrix_object)
#[,1] [,2]
#[1,]    3    7
#[2,]    1    5

cacheSolve(myMatrix_object)
#getting cached data
#[,1] [,2]
#[1,]    3    7
#[2,]    1    5
