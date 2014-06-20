## cacheMatrix:
## A special kind of matrix which can store its inverse
## in a cache to save on computation.

## makeCacheMatrix:
## Returns a 'matrix' object which supports the following:
## m$set(x) -- set the value of the matrix
## m$get(x) -- retrieve the value of the matrix
## m$setinverse(x) -- set the cached inverse
## m$getinverse(x) -- retrieve the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve:
## Returns the inverse of a cacheMatrix by looking
## in the cache for a stored value to save on
## expensive computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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