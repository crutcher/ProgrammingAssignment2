## Put comments here that give an overall description of what your
## functions do

## Construct a new "cache" matrix object.
#
# The resulting object will have 4 bound operations:
#  x$set() - set the matrix value, reset the inverse.
#  x$get() - get the matrix value.
#  x$setinv() - set the cached matrix inverse.
#  x$getinv() - get the cached matrix inverse, or NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv 
  getinv <- function() m
  
  list(
    set=set,
    get=get,
    setinv=setinv,
    getinv=getinv
  )
}

## Extract or compute the inverse of the given "cache" matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    return(m)
  }
  
  m <- solve(x$get())
  x$setinv(m)
  m
}
