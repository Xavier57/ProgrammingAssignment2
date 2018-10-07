## Put comments here that give an overall description of what your
## functions do:

##################################################################################
# Matrix inversion is usually a costly computation and there may be some benefit #
# to caching the inverse of a matrix rather than compute it repeatedly. The      #
# following two functions are used to cache the inverse of a matrix.             #
##################################################################################

## Write a short comment describing this function:

##################################################################################
#`This function creates a special "matrix" object                                #
#that can cache its inverse.                                                     #
##################################################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #setting the value of the matrix
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }

  #getting the value of the matrix
  get <- function() x

  #setting the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  #getting the value of inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function:

##################################################################################
# The function cacheSolve returns the inverse of a matrix A created with         #
# the makeCacheMatrix function.                                                  #
# If the cached inverse is available, cacheSolve retrieves it, while if          #
# not, it computes, caches, and returns it.                                      #
##################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  # if the inverse has already been calculated
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    # get it from the cache and skips the computation.
    return(inv_x)
  } else {
    # otherwise, calculates the inverse 
    inv_x <- solve(x$get())
    # sets the value of the inverse in the cache via the setinverse function.
    x$setinverse(inv_x)
    return(inv_x)
  }
}
