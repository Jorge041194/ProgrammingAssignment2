## Functions to create and cache the inverse of a matrix

## Define functions for setting and getting matrix and inverse

makeCacheMatrix <- function (x = matrix()) {
      inversa <- NULL
      set <- function (y) {
           x <<- y
           inversa <<- NULL
      }

      get <- function () {x}
      setinversa <- function(inv_calc){ inversa <<- inv_calc }
      getinversa <- function() { inversa }
      list( set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}


## Determine if inverse matrix is already cached, solve if not. Return
## inverse matrix


cacheSolve <- function (x, ...) {
 ## Return a matrix that is the inverse of 'x'

    inversa <- x$getinversa()
    if ( is.null(inversa) ) {
        message ( "getting cached data" )
        return ( inversa )
    }
    data <- x$get()
    inversa <- solve(data, ...)
    x$setinversa(inversa)
    inversa
}