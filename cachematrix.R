##
## Defines a CacheMatrix object which caches the matrix inverse to improve efficiency
## The underlying matrix can be accessed via a function interface
## Usage: cachedMatrix <- makeCacheMatrix(matrix)
##        cachedMatrix$get()
##        cacheSolve(cachedMatrix)
##

## Builds a CacheMatrix from a standard matrix
## Returns an interface in the form of functions in a list
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the matrix inverse for a CacheMatrix, using its caching feature
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}

