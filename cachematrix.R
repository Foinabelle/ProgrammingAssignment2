## The makeCacheMatrix and cacheSolve functions calculate the inverse of a 
## matrix and store it so that it can be accessed rather than recalculated 
## each time it is required.  

## makeCacheMatrix builds a set of functions and returns them as a list
## that is named and can therefore be accessed using the $. 

makeCacheMatrix <- function(x = matrix()) {m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinv <- function(solve) m <<- solve
getinv <- function() m
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}


## CacheSolve calculates and stores the inverse of the matrix by accessing
## the functions from makeCacheMatrix.

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
