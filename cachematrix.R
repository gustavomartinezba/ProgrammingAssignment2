## These two functions cache the computation of the
## inverse of (invertible) matrices

## This function creates a special "matrix", which is
## really a list with the setters and getters

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get, setinv = setinv,
           getinv = getinv)
}


## This functions calculates (or gets from cached data if 
## possible) the inverse of the special "matrix" created
## above

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i  ## Return a matrix that is the inverse of 'x'
}
