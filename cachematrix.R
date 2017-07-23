## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## This assignment is to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  # 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  # 2. get the value of the matrix
  get <- function() x
  # 3. set the value of inverse of the matrix
  setinv <- function(inv) inverse1 <<- inv
  # 4. get the value of inverse of the matrix
  getinv <- function() inverse1
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse2 <- x$getinv()
  ## for retrieving cache from previous runs of the function
  if(!is.null(inverse2)) {
    message("getting cached data")
    return(inverse2)
  }
  data <- x$get()
  ## using solve function to find inverse
  inverse2 <- solve(data)   
  x$setinv(inverse2)
  inverse2
}
