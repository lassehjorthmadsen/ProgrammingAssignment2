## Two functions, makeCacheMatrix and cacheSolve used for calculating
## the inverse of a matrix, then caching the results for possible reuse.

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a cached object for a matrix to be inverted
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'

  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
