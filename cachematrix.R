# this function is similar to a class. it creates a list
# that contains 4 member functions: set, get, setInv
# and getInv. The <<- assignment operator allows that 
# internal variables are not exposed to the outside environment. 

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL # result of inversion is stored
  # A setter function, use this to set a matrix to object created by makeCacheMatrix function
  # makeCacheMatrix(testmatrix) # here we work on testmatrix
  set <- function(y) {
    x <<- y
    xInverse <<- NULL 
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xInverse <<- inv # set the inversed matrix
  getInv <- function() xInverse # return the inversed matrix
  # return a list that contains these functions, so that we can use
  # makeCacheMatrix object like these
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xInverse <- NULL" in the previous function
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result
}
