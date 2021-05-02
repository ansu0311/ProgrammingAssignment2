## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data) %*% data
    x$setInverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
