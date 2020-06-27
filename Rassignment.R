makeMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { ##this function set the matrix value
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  cacheSolve <- function() inverse ## This function create the inverse of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       cacheSolve = cacheSolve)
}

makeCacheMatrix <- function(x, ...) {
  inverse <- x$cacheSolve()
  if(!is.null(inverse)) {
    message("getting cached data") ## If inverse of matrix is present in cache then it returns the matrix inverse
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}