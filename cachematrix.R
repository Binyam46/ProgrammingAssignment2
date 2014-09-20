# This function creates a special "matrix"
# The input of the function is an invertible matrix
# The outputs of the function are list of functions, which are used to set
# a the value of a matrix, get the value of a matirx, set the inverse of
# the matrix and get the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  InvM <- NULL
  
  # this function set the value of the matrix
  setM <- function(y) {
    m <<- y
    InvM <<- NULL
  }
  
  # this function get the value of the matrix
  getM <- function() m
  
  # this function set the inverse of the matrix 
  setInvM <- function(Inv) InvM <<- Inv
  
  # This function get the inverse of the matrix
  getInvM <- function() InvM
  
  # return a list of functions (output of the function)
  list(setM = setM, getM = getM,
       setInvM = setInvM,
       getInvM = getInvM)
}


# This function calculates the inverse of a special "matrix" created by 
# 'makeCacheMatrix.R' function
# The function first checks if the inverse of the matrix is already calculated
# If the inverse exists, it get the inverse from the cache. If not, it calculates 
# the inverse of the matrix and pass it to cache through the 'setInvM' function.
# The input of the function is the special "Matrix"
# the output of the function is inverse of the matrix (either from cache or calculated)

cacheSolve <- function(m, ...) {
  
  # this line get the inverse of the matrix
  InvM <- m$getInvM()
  
  # This checks if the inverse of the matrix is already calculated. If so, it skips the 
  # rest of the commands and return the inverse
  if(!is.null(InvM)) {
    message("getting cached Inverse")
    return(InvM)
  }
  
  # this line get the data (matrix)
  dataM <- m$getM()
  # this line calculates the inverse of the matrix
  InvM <- solve(dataM, ...)
  # this line pass the inverse to cache 
  m$setInvM(InvM)
  # return the inverse (output of the function)
  InvM
}
