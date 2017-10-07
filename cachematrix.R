## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix and cacheSolve together work to remove the 
# computational overhead of computing the inverse of a matrix if 
# the value has already been computed
# The first time an inverse is computed, it is cached such that the 
# next time it is requested, no computation is done but the cached value
# is returned

## Write a short comment describing this function

# makeCacheMatrix creates a special matrix that caches its inverse
# Args:
#   x: An invertible matrix
# Returns:
#   A list of functions to:
#     - retrieve the matrix
#     - set its value
#     - set its inverse
#     - get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

# cacheSolve computes the inverse of a matrix only if it was not computed before
# if it was, the already computed value is returned
# Args:
#   x: A special matrix created by makeCacheMatrix
# Returns:
#   The computed or cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
