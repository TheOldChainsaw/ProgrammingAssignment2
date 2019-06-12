## Solves and returns the inverse of matrices,
## caching data to be returned if called again

## Creates a special "matrix" object to cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Solves and caches the inverse of a matrix or returns
## the inverse from the cache if it has been solved before

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
