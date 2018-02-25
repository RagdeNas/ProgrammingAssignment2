## Two functions that cache the calculation of the inverse of a matrix

## The following one saves the matrix data and the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
     x <<- y
     m <<- NULL
  }
  get <- function() x
  setInverse <- function( solve ) m <<- solve
  getInverse <- function() m
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following one checks if the matrix's inverse is already in cache, else, performs the solve

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve( data,... )
  x$setInverse(m)
  m
}
