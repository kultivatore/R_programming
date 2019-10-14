## Caching the inverse of a given matrix:
##Computing the inverse of the matrix could be a costly computation so it is better to catching the inverse
## The functions below are basically used to create a special object that 
## stores a matrix and caches its inverse.

## The following function creates the matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix created by 
## the "makeCacheMatrix" code above. If the matrix has not changed and the inverse 
##has been calculated before, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return the inverse matrix
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
