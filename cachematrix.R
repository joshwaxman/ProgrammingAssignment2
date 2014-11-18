## makeCacheMatrix and cacheSolve work together. 
## * cacheSolve computes the inverse of a matrix. However, this 
## can be a time-consuming operation. 
## * makeCacheMatrix will create a special matrix object which will
## cache its inverse. specifically, it is a function that effectively 
## creates a class with getters and setters and which stores the result
## of the inverse operation, which can be used assuming the input
## matrix has not changed.

## makeCacheMatrix will create a class for long-term storage of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this will compute the inverse of the matrix, but will rely on the cache
## of the inverse, if available, rather than recomputing

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## instructions to the evaluator for how to use these functions.
## issue the following commands at the R console:
##
## source("cachematrix.R")
## mat = array(c(3, 4, 1, 1, 3, 1, 3, 5, 1), dim=c(3,3))
## mat
## j <- makeCacheMatrix()
## j$set(mat)
## cacheSolve(j)
## solve(mat)
##
## That is, load the R program; create a matrix to solve; print that matrix;
## create a new cacheMatrix called j; set its matrix to compute to be the
## same at mat; cacheSolve j; visually compare the results with solving mat.