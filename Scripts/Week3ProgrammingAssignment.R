#clear the environment before executing code:
remove(list = ls())


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: Creates an object that has setters and getters, as well as cache for storing the latest inverse of a matrix, as well as the matrix.
# Parameters: x: accepts a matrix data structure. 

 makeCacheMatrix <- function(x = matrix()) {
    
   # Variables inside the object
   
   inverseMatrix <- NULL #variable to store the inverse of the matrix
   
   # Getters and Setters for the Matrix variable of the object
   getMatrix <- function()x # getter to retrieve the passed in matrix

   
   # Getters and Setters for the Inverse Matrix variable of the object
   getInverseMatrix <- function()inverseMatrix
   setInverseMatrix <- function(y = matrix())
   {
     inverseMatrix <<- y
   }
    
   list(getMatrix = getMatrix,
        getInverseMatrix = getInverseMatrix,
        setInverseMatrix = setInverseMatrix)
         
   
 }
 
 # ## Write a short comment describing this function
 # cacheSolve: Used to check if the object being supplied, already has an inverse matrix cached. If the object has not inverse, then a new inverse matix is created.
 # Parameters: x: accepts makeCacheMatrix object 
 storeCurrentMatrix <- NULL
 cornercaseCacheStateSaver <- NULL
 cacheSolve <- function(x = makeCacheMatrix()) {
   
   getOldMatrix <- function()storeCurrentMatrix # getter to retrieve formerly processed matrix
   
   setCornerCaseStateSaverMatrix <- function(y = matrix()) # setter to store matrix/formerly processed matrix
   {
     cornercaseCacheStateSaver <<- y
     
   }
   setMatrix <- function(y = matrix()) # setter to store matrix/formerly processed matrix
   {
     storeCurrentMatrix <<- y
     
   }
   
   processCache <- function(matrixtoprocess = matrix())
   {
     #Check if the matrix is invertible or singular
     if(det(matrixtoProcess) == 0)
     {
       print("The matrix is singular, and has no inverse")
       
     }
     else   
     {
       print("1:Calculating the inverse of the matrix")
       x$setInverseMatrix(solve(matrixtoProcess))
       setCornerCaseStateSaverMatrix(x$getInverseMatrix())
       setMatrix(matrixtoProcess)
       x$getInverseMatrix()
     }  
   }
   
   matrixtoProcess <- x$getMatrix()   # Set the matrix to ptocess
   oldmatrixProcessed <- getOldMatrix() # Get old matrix
   
   
   #If there is no old matrix that was processed in the past
   # perform the inverse calculation
   if(is.null(oldmatrixProcessed)) 
   {
     
      processCache(matrixtoProcess)
     
   }
   #If old matrix was already present, check if the new matrix and old matrix are the same, if not
   # recalculate a new inverse
   else #Step1: check if rows and coulmns are same or different
   {
     if(nrow(matrixtoProcess) != nrow(oldmatrixProcessed))
     {
       print("The matrix has changed")
       print("Calculating the inverse of the matrix")
       processCache(matrixtoProcess)
     }
     else if(ncol(matrixtoProcess)!=ncol(oldmatrixProcessed))
     {
       print("The matrix has changed")
       print("Calculating the inverse of the matrix")
       processCache(matrixtoProcess)    
     }
     else #Step2: Check if contents of the both matrices are the same
     {
       matdiff <- sum(as.integer(matrixtoProcess==oldmatrixProcessed))
       if(matdiff == nrow(matrixtoProcess)*ncol(matrixtoProcess))
       {
         print("The matrices are equal; retrieving inverse from the Cache")
         if(is.null(x$getInverseMatrix())) #Corner case: intializing the same object twice in a row, with same content.
         {
           #print("Corner case: retiriving from the Central Cache: which is good Caching design")
           #print(cornercaseCacheStateSaver)
           x$setInverseMatrix(cornercaseCacheStateSaver)
         }
         else #Step3: As long as the dimensions and the contents of both matrices match, fetch from the cache
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
 mat1 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat1)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 

 #Scenario 2: Passing in the same object to exercise caching capability
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 #Scenario 3: Passing in a new matrix vector to check, if Caching logic recognizes the change
 
 mat2 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = FALSE)
 resultmatrix <- makeCacheMatrix(mat2)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 #Scenario 4: changing the dimensions and values of the matrix, to check if Caching logic recognizes the change
 # Also passing in a singular matrix, to check if the code can handle such a case
 mat2 <- matrix(2:17,4,4)
 resultmatrix2 <- makeCacheMatrix(mat2)
 cacheVar <- cacheSolve(resultmatrix2)
 
 #Scenario 5: Re-initiazing and passing in the same object again and again, to check if the Cache is optimized to handle
 # such a scenario
 
 mat1 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat1)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 mat1 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat1)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 mat1 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat1)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)

 #scenario 6: Passing in the same matrix via different objects to see , if caching still optimizes that scenario
 mat1 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat1)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 mat2 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat2)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
 
 mat3 <- matrix(c(1,0,4,1,3,4,4,1,0),3,3,byrow = TRUE)
 resultmatrix <- makeCacheMatrix(mat3)
 cacheVar <- cacheSolve(resultmatrix)
 print(cacheVar)
