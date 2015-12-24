# makeCacheMatrix is a list of functions
# to cache the inverse of a square-invertable-matrix.

# set(), get(), setinverse(), getinverse()
# are the functions in "makeCacheMatrix" output list.

makeCacheMatrix <- function( x = matrix() ) {
  
  matrix_inverse <- NULL
  
  set <- function( set.matrix
                   , computed.inverse ) {
    
    x <<- set.matrix
    
    matrix_inverse <<- computed.inverse
  }
  
  get <- function() x
  
  setinverse <- function(to_invert) matrix_inverse <<- solve(to_invert)
  
  getinverse <- function() matrix_inverse
  
  list( set = set
        , get = get
        , setinverse = setinverse
        , getinverse = getinverse )
}

# "cacheSolve" input is  "makeCacheMatrix( square-invertable-matrix )",
# output is a inverse of the square-invertable-matrix.

# "explicit.matrix" and "computed.inverse"
# are two more arguments for "cacheSolve"

# A matrix can be set explicitly through input "explicit.matrix"
#("explicit.matrix" is optional )
  
# If the inverse has already been calculated,
# then that is input through "computed.inverse"
# Matrix inversion is usually a costly computation.  

## Also, one can check Whether a matrix is inverse of itself,
## but I have not included this functionality.
##if( !is.null( matrix_inverse && 
##              x$get() %*% x$get()
##              == diag( matrix(0, n, n) ) <- 1 )) {

cacheSolve <- function( x = makeCacheMatrix()
                        , explicit.matrix = NULL
                        , computed.inverse = NULL ){
#  
  if( !is.null(explicit.matrix) ) x$set( explicit.matrix
                                         , computed.inverse  )
  
  matrix_inverse <- x$getinverse()
  
  if( !is.null( matrix_inverse )) {
##    
    message("getting cached data")
    
    return( matrix_inverse )
  }
  
  to_invert <- x$get()
  
  x$setinverse(to_invert)
  
  matrix_inverse <- x$getinverse()
  
  matrix_inverse
  
  }
