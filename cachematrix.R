## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The 2 sets of functions below were used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

##Function 1

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


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix in the previous step. In case the inverse has already been calculated (and the
## matrix does not change), then the function retrieves the inverse from the cache.

##Function 2

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
