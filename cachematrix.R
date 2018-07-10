## Title: R-Programming Course, Week 3 - Second Programming Assignment
## Author: Andrew Yoon
## Two functions are used to calculate an inverse of a matrix 
## whenever the value changes and pull the cached value if the 
## value has not been changed. 
#######################################################################
## This function creates a special "matrix" object that can cache its inverse.
cacheSolve <- function(x = matrix()) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), the the cacheSolve should retrieve 
## the inverse from the cache. 

makeCacheMatrix <- function(x = matrix()) {
  ## return a matrix that is the inverse of 'x'
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
