makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# The below function calculates the inverse of the special “matrix” returned by makeCacheMatrix function above. If the inverse has already been calculated, then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

B <- matrix(c(1,2,3,4),2,2)


B1 <- makeCacheMatrix(B)
cacheSolve(B1)
cacheSolve(B1) 
