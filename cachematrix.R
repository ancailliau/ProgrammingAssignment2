## Functions to cache matrix inversion

## Creates a special "matrix" object that can cache its inverse.
##
## Pre: If provided, `x` is an invertible matrix
## Post: Returns a special "matrix" object corresponding to the specified matrix `x`.
##       if no matrix 'x' is provided, returns a special "matrix" object corresponding
##                                     to the 1-by-1 matrix containing NA

makeCacheMatrix <- function(cachedMatrix = matrix()) {
  cachedInverse <- NULL # cache for the inverse of the matrix

  # 'set' function to update the represented matrix
  set <- function(newMatrix) {
    cachedMatrix <<- newMatrix
    cachedInverse <<- NULL # invalidate cached inverse
  }

  # 'get' function to retreive the represented matrix
  get <- function() cachedMatrix

  # 'setinverse' function to cache the inverse
  setinverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix

  # 'getinverse' function to retreive cached inverse
  getinverse <- function() cachedInverse

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix`
##
## Pre: `x` is a special "matrix" build using `makeCacheMatrix`
## Post: Returns the inverse matrix of `x`.
##       If inverse has already been computed using cacheSolve, returns the pre-computed value.
##       If inverse has not been already computed, caches the inverse.
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getinverse()
  if(!is.null(cachedInverse)) {
    message("Getting inverse from cache")
    return(cachedInverse)
  }
  matrix <- x$get()
  cachedInverse <- solve(matrix, ...)
  x$setinverse(cachedInverse)
  cachedInverse
}
