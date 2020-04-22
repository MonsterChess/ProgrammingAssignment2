## This program contains two functions
## makeCacheMatrix will cache the inverse of a given matrix
## cacheSolve will check to see if the solution is cached

## Get and set the values of the matrix and the cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) invmat <<- solve
  getsolve <- function() invmat
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x', using cached inverse when possible
cacheSolve <- function(x, ...) {
  invmat <- x$getsolve()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setsolve(invmat)
  invmat
}
