## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object that can cache a copy of its inverse.  Actually,
## the code is pretty generic and could be used to cache any function of the original matrix 
## that results in a matrix.  This function is basically the same as the makeVector() function
## given in the description of this assignment but with different variable and function names. 

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) im <<- inverse
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function is very similar to the 
##  1. retrieves the matrix stored in the matrix object argument.
##  2. checks that it is a square matrix since we can only compute the inverse of square matrices.
##  3. checks to see of the matrix's inverse is already cached in the matrix object
##  4. if object's cache is empty, computes the inverse and caches it.
##  5. returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m = x$get()
  dimensions = dim(m)
  if (dimensions[1] != dimensions[2]) {
    message("ERROR: must be a square matrix")
    return(NULL)
  }
  inverse = x$getInverse()
  if (!is.null(inverse)) {
    message("returning cache data")
  } else {
    inverse = solve(m)
    x$setInverse(inverse)
  }
  return(inverse)
}
