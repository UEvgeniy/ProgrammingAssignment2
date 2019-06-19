## Put comments here that give an overall description of what your
## functions do


## Function creates a special "matrix" object that can cache its inverse. Has getters and setters for own matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  set_inversed <- function(inversed) i <<- inversed
  get_inversed <- function() i
  list(set = set,
       get = get,
       set_inversed = set_inversed,
       get_inversed = get_inversed)
}


## Function returns cached inverse or computes it first time. Contains logging messages for testing correct behavior.

cacheSolve <- function(x, ...) {
  i <- x$get_inversed()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  message("first computation")
  data <- x$get()
  i <- solve(data, ...)
  x$set_inversed(i)
  i
}
