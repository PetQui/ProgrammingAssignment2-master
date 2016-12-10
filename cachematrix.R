## Put comments here that give an overall description of what your
## functions do
## Returning the inversion of a matrix or an error message if the matrix input matrix is not square 
## or if the matrix is not invertable.

## Write a short comment describing this function
## Called to initialize the input matrix and exposing functions
## to reset the value of the input matrix (set), retrieve the value of the matrix(get), 
## cache the inversion of the input matrix (setinverse),and to retrieve the inversion (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x) != ncol(x)) {print("error: input matrix must be square")}
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinv = setinverse,
       getinv = getinverse)
}


## Write a short comment describing this function
## Returns the inversion of the input matrix stored by makeCachMatrix either by 
## calculating and caching it, if not available in cache, or by retrieving it from
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
