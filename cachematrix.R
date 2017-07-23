## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse1 <<- inv
  getinv <- function() inverse1
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse2 <- x$getinv()
  if(!is.null(inverse2)) {
    message("getting cached data")
    return(inverse2)
  }
  data <- x$get()
  inverse2 <- solve(data)
  x$setinv(inverse2)
  inverse2
}
