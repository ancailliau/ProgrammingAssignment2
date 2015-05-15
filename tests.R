source("cacheMatrix.R", echo=F)

# Helper function to compare two matrices
matequal <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}


# Test 01 : Test makeCacheMatrix with default argument
cat("test01: Test without arguments\n")
x <- makeCacheMatrix()
temp <- cacheSolve(x)
temp <- cacheSolve(x) # shall display 'Getting inverse from cache'
cat("-> Shall display 'Getting inverse from cache' only once\n\n")


# Test 02 : Test makeCacheMatrix with specified matrix
cat("test02: Test with arguments\n")
m <- matrix(sample(1:100, 1000 * 1000, replace=T), nrow=1000)
x <- makeCacheMatrix(m)
temp <- cacheSolve(x)
temp <- cacheSolve(x) # shall display 'Getting inverse from cache'
cat("-> Shall display 'Getting inverse from cache' only once\n\n")


# Test 03 : Test set function
cat("test03: Test with set\n")
x <- makeCacheMatrix()
temp <- cacheSolve(x)
m <- matrix(sample(1:100, 1000 * 1000, replace=T), nrow=1000)
x$set(m)
temp <- cacheSolve(x) # shall *not* display 'Getting inverse from cache'
temp <- cacheSolve(x) # shall display 'Getting inverse from cache'
cat("-> Shall display 'Getting inverse from cache' only once\n\n")


# Test 04 : Test get function
cat("test04: Test get\n")
m <- matrix(1)
x <- makeCacheMatrix(m)
result <- matequal(x$get(), m)
m <- matrix(sample(1:100, 1000 * 1000, replace=T), nrow=1000)
x$set(m)
result <- result && matequal(x$get(), m)
if (result) {
  cat("-> Succeed\n")
} else {
  cat("-> Failed\n")
}

