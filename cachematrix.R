## This function creates a special "matrix" object that can cache its inverse
## using R "solve" function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # assign matrix to x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve # calculate inverse
  getInverse <- function() m
  # returning list of functions, i.e., get, set, and setInverse and getInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  }


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  # if cachecd data found return that otherwise calculate inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}