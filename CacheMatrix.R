makeCacheMatrix <- function(m = matrix()) {
  invm <- NULL
  set <- function(n) {
    m <<- n
    invm <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  invm <- m$getInverse()
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  matrik <- m$get()
  invmatrik <- solve(matrik, ...)
  m$setInverse(invmatrik)
  invm
}