## 2 functions have been defined makeCacheMatrix() and cacheSolve().
## These will be used to compute and cache the inverse of a given invertible matrix
## as required in the assignement.

## The first function is called make CacheMatrix(). This function takes as an invertible
## matrix. It will create a list of functions that will :
## a) set the value of the matrix - set()
## b) get the value of the matrix - get()
## c) set the value of the inverse of the matrix - setinv()
## d) get the value of the inverse of the matrix - getinv()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will calculate the inverse of the given invertible matrix.
## Inverse is calculated and stored in the cache memory.
## If inverse is already calculated then it is retrieved from cache memory without
## having to recalculate the inverse, thereby saving precious resources.
## Variable "m" will store the value of the inverse of the 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
