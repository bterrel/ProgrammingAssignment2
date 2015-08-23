## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly.
## These functions compute and cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  # Step1: set the matrix
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Step2: get the matrix
  get = function() x
  
  # Step3: set the inverse
  setinv = function(inverse) inv <<- inverse 
  
  # Step4: get the inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  # if the inverse has already been calculated (and the matrix has not changed)
  # then cacheSolve should retrieve the inverse from the cache.
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # else, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  
  x$setinv(inv)
  return(inv)
}
