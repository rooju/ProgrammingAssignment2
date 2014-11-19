## These set of functions create a matrix-inverse calculator with support for caching.
## Function 'makeCacheMatrix' - Creates a special "matrix inverse cache" object
## Function 'cacheSolve' - Calculates the inverse of a matrix given to it.

## -----------------------------------------------------------------------------
## Function
## -----------------------------------------------------------------------------
## makeCacheMatrix : Creates an object that represents a matrix and the cached
##                   and the cached value of it's inverse.
## -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
    # The variable 'matinv' represents the cached matrix inverse value.
    # Initialize it to NULL.
    matinv<-NULL
    
    # The function 'setmatrix' sets the matrix for which the inverse is computed.
    setmatrix<- function(y) {
      # Set matrix
      x<<-y
      
      # Reset inverse
      matinv<<-NULL
    }
    
    # The function 'getmatrix' gets the matrix for which the inverse is computed.
    getmatrix<- function() { x }
    
    # The function 'setinverse' sets the value of inverse that is to be cached.
    setinverse<- function(i) {
      matinv<<-i
    }
    
    # The function 'getinverse' returns the cached value of the computed inverse.
    getinverse<-function() { matinv }
    
    list(set = setmatrix, get = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

## -----------------------------------------------------------------------------
## Function
## -----------------------------------------------------------------------------
## cacheSolve : Computes the inverse of a matrix represented by 'x' & caches it.
##              Assumes that the matrix is invertible (uses 'solve' instead of 'ginv')
## -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  
  # If cached value of inverse exists, return it.
  
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  
  # No cached value of inverse found.  Calculate it.
  
  # If matrix is not set, stop.
  
  m <- x$get()
  if (is.null(m)) {
    stop("Cannot solve. No matrix set")
  }
  
  # Compute inverse using 'solve'.  Note: 'solve' computes true inverse.
  # By using 'solve', this assignment assumes that the given matrix is invertible.
  # All practical implementations must use ginv from the MASS package.
  i <- solve(m)
  
  # Cache the inverse.
  
  x$setinverse(i)
  
  i  
}
