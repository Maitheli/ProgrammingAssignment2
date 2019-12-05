## Functions to get the inverse of a matrix
## The inverse is cached for a given input matrix
## If called again wih the same input matrix, the inverse wouldn't be computed, but queried from the cache.

## Function makeCacheMatrix
## set the value of the input matrix
## get the value of the input matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  getmatrix <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## Function cacheSolve - to get the inverse of the input matrix
## inverse of input matrix in cache -> return cached inverse
## inverse of input matrix not in cache -> return computed inverse and cache it

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Retrieving Cached Matrix Inverse.")
    return(inverse)
  }
  mat <- x$getmatrix()
  inverse <- solve(mat)
  x$setinverse(inverse)
  inverse
}
