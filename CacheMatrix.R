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
 
  invm <- m$getInverse()
  if (!is.null(invm)) {
    message("data")
    return(invm)
  }
  matrik <- m$get()
  invmatrik <- solve(matrik, ...)
  m$setInverse(invmatrik)
  invm
  
}
