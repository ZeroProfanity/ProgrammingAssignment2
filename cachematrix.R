## In order to reduce calculation time on performing the same matrix
## inversion twice, the functions below allow for a inverted matrix
## to be cached. Only the first inversion by cacheSolve(x) will 
## execute a calculation where all subsequent inversions of the same 
## matrix will result in a read from the cache.

## Converts a matrix to a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Reads a previously cached inverted cacheable matrix if this 
## matrix is available in cache. If this is not the case, the  
## cacheable matrix is inverted and committed to cache for future
## reads.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
