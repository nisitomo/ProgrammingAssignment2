##Overall description of this R file:
##Below are a pair of functions that cache the inverse of a matrix
##(makeCacheMatrix and cacheSolve)

##This first function below, makeCacheMatrix,
##creates a special 'matrix' to
##(1) set the value of the matrix,
##(2) get the value of the matrix,
##(3) set the value of the matrix's inverse, and
##(4) get the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inversedMatrix) inv <<- inversedMatrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


##The next function, cacheSolve, calculates the inverse
##of the special 'matrix' created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix
##and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
