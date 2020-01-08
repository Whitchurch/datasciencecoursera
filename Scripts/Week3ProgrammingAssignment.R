
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: Creates an object that has setters and getters, as well as cache for storing the latest inverse of a matrix, as well as the matrix.
# Parameters: x: accepts a matrix data structure.

makeCacheMatrix <- function(x = matrix()) {
  # Variables inside the object
  
  inverseMatrix <-
    NULL #variable to store the inverse of the matrix
  
  # Getters and Setters for the Matrix variable of the object
  getMatrix <-
    function()
      x # getter to retrieve the passed in matrix
  
  
  # Getters and Setters for the Inverse Matrix variable of the object
  getInverseMatrix <- function()
    inverseMatrix
  setInverseMatrix <- function(y = matrix())
  {
    inverseMatrix <<- y
  }
  
  list(
    getMatrix = getMatrix,
    getInverseMatrix = getInverseMatrix,
    setInverseMatrix = setInverseMatrix
  )
  
  
}

# ## Write a short comment describing this function
# cacheSolve: Used to check if the object being supplied, already has an inverse matrix cached. If the object has not inverse, then a new inverse matix is created.
# Parameters: x: accepts makeCacheMatrix object
storeCurrentMatrix <- NULL
cacheSolve <- function(x = makeCacheMatrix()) {
  getOldMatrix <-
    function()
      storeCurrentMatrix # getter to retrieve formerly processed matrix
  
  setMatrix <-
    function(y = matrix())
      # setter to store matrix/formerly processed matrix
    {
      storeCurrentMatrix <<- y
      
    }
  
  matrixtoProcess <- x$getMatrix()   # Set the matrix to ptocess
  oldmatrixProcessed <- getOldMatrix() # Get old matrix
  
  calculateInverse <- function()
  {
    setMatrix(x = matrix())
    
    #Check if the matrix is invertible or singular
    if (det(getOldMatrix()) == 0)
    {
      print("The matrix is singular, and has no inverse")
      x$setInverseMatrix <- 0
    }
    else
    {
      print("Calculating the inverse of the matrix")
      x$setInverseMatrix(solve(getOldMatrix()))
    }
  }
  
  #If there is no old matrix that was processed
  # perform the inverse calculation
  if (is.null(oldmatrixProcessed))
  {
    calculateInverse(matrixtoProcess)
    
  }
  #If old matrix was already present, check if the new matrix and old matrix are the same, if not
  # recalculate a new inverse
  else
  {
    if (nrow(matrixtoProcess) == nrow(oldmatrixProcessed) &
        ncol(matrixtoProcess) == ncol(oldmatrixProcessed))
    {
      matdiff <- sum(as.integer(matrixtoProcess == oldmatrixProcessed))
      if (matdiff == nrow(matrixtoProcess) * ncol(matrixtoProcess))
      {
        print("The matrices are equal; retrieving inverse from the Cache")
        print(x$getInverseMatrix())
      }
      else
      {
        print("The matrix has changed")
        print("Calculating the inverse of the matrix")
        calculateInverse(matrixtoProcess)
      }
    }
    # If old and new matrices are not the same calculate new inverse
    else
    {
      calculateInverse(matrixtoProcess)
    }
    
    
  }
  
  
  #   ## Return a matrix that is the inverse of 'x'
}

mat1 <- matrix(c(1, 0, 4, 1, 3, 4, 4, 1, 0), 3, 3, byrow = TRUE)
resultmatrix <- makeCacheMatrix(mat1)
cacheSolve(resultmatrix)

mat2 <- matrix(2:17, 4, 4)
resultmatrix2 <- makeCacheMatrix(mat2)
cacheSolve(resultmatrix2)
