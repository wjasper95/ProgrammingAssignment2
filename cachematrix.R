## Put comments here that give an overall description of what your
## functions do

# Quite similar to makevector() and cachemean(), my two functions also allow 
# caching i into the parent environment with <<-, except it takes a matrix as an 
# input and caches its inverse. Cachesolve() will return this cached inverse if it is
# available, and if it isn't, it will calculate it! Thanks for reading!

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object that can cache its 
# inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(m_inv) i <<- m_inv
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## Write a short comment describing this function

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache!

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
