## This pair of functions cache the inverse of a matrix. 
## Since Matrix inversion can be a costly computation, there
## is a benefit to caching the inverse of a matrix.

## makeCacheMatrix: a function that creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }

## 'cacheSolve' computes the inverse of the 
## matrix returned by 'makeCacheMatrix'. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

