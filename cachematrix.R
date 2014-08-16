## Cache a matrix inverse.
## Example:
##   > mtx <- makeCacheMatrix(matrix(1:4,2,2))
##   > cacheSolve(mtx)
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##   > cacheSolve(mtx)
##   getting cached data
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5


## function makeCacheMatrix
## Create an object that encapsulates a matrix and caches the inverse of it.
## Used in conjunction with cacheSolve
## Arguments:
##   x: A matrix that has an inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## function cacheSolve
## Solve the inverse of a matrix, encapsuled by makeCacheMatrix.
## If the function is called repeatedly for the same cached matrix,
## then the cached inverse is returned rather than being recalculated.
## Arguments:
##   x: an encapsuled matrix, created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
