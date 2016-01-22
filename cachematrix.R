## Defines functions that cache the inverse of a matrix

## Creates a "matrix" object that can cache its inverse.
## It uses the joint enclosing environment of the created functions to store the state,
## which consists of the matrix and the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the "matrix" object defined using the makeCacheMatrix.
## If the inverse already has been computed before, it is returned immediately.
## If the inverse has not been computed before, it is computed and cached.
## The function assumes that the matrix x is invertible; no error handling is performed.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
