makeMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  cacheSolve <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       cacheSolve = cacheSolve)
}

makeCacheMatrix <- function(x, ...) {
  inverse <- x$cacheSolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}