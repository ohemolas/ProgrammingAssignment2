## Put comments here that give an overall description of what your
## functions do
## Creates a matrix, then caches and computes the inverse of the matrix

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object 'x' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Initialize
  set <- function(y) {  
      x <<- y
      m <<- NULL
  }
  get <- function() x   
  setinv <- function(inv) m <<- inv  
  getinv <- function() m  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## Write a short comment describing this function
## CacheSolve computes the inverse of the matrix 'x' returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
## Return a matrix that is the inverse of 'x'
