## Put comments here that give an overall description of what your
## functions do

## This function is basically the same as the makeVector() function given in the description of this assignment
##
## makeCacheMatrix(x) where 'x' is a matrix creates a matrix object instance with methods  
## to fetch and save a copy of its inverse.  Actually, the code is pretty generic and could be used 
## to cache any function of the original matrix that results in a matrix.  
## but with different variable and function names. 

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


## This function is very similar to the cachemean() function given in the assignment description.
##
## cacheSolve(x) where 'x' is a matrix object instance created by makeCacheMatrix()
##  1. retrieves the matrix stored in the matrix object argument.
##  2. checks that it is a square matrix since we can only compute the inverse of square matrices.
##  3. checks to see of the matrix's inverse is already cached in the matrix object
##  4. if object's cache is empty, computes the inverse and caches it.
##  5. returns the inverse.
##  I check for a square matrix but there are square matrices that do not have inverses.
##  this function will err out at the solve() call if given one of these matrices.

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
    inverse = solve(m, ...)
    x$setInverse(inverse)
  }
  return(inverse)
}
