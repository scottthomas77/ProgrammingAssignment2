## This file contains two functions:
## 1.  makeCacheMatrix - creates a special 'matrix' that can cache it's inverse matrix
## 2.  cacheSolve - calculates the inverse of the matrix and caches it

## makeCacheMatrix - creates a special 'matrix' that can cache it's inverse matrix
## This 'matrix' is actually a list containing the following functions:
##
##  1.  get - returns the matrix 'x'
##  2.  set - set the value of the matrix 'x'
##  3.  getinverse - get the inverse of the matrix 'x'
##  4.  setinverse - set the inverse of the matrix 'x'
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## stores the inverse of the matrix 'x'
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves for the inverse of the special matrix
## created by makeCacheMatrix.  It first checks to see if the 
## inverse matrix is already in the cache.  If the inverse
## is found it returns the cached value, otherwise it will
## calculate and cache the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Check to see if the inverse matrix has already been cached
  i <- x$getinverse()
  
  ## If it is cached, simply return the cached value
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Otherwise, calculate the inverse of the matrix,
  originalMatrix <- x$get()
  i <- solve(originalMatrix) # aalculate the inverse
  x$setinverse(i) # cache the value for future reference
  
  i ## finally, return the inverse of the matrix 'x'
}
