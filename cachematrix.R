## These functions, when combined, extract the inverse of a given matrix and cache the result
## so that if there is a new call repeated, the result can be retrieved from the cache rather
## than unnecessarily recalculated.

## This function assembles a list of functions that will allow to store the inverse matrix,
## once calculated.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function tests the existence of the desired result in cache, retrieving it if positive,
## or calculating and storing it if negative.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {        ## Testing the existence of the desired result in cache
    message("Getting cached data...")
    return(inv)       ## Retrieving the result
  }
  data <- x$get()
  inv <- solve(data)        ## Calculating de inverse matrix
  x$set_inverse(inv)        ## Storing the result in cache
  inv
}
