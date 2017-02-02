## Functions to calculate and cache the inverse of a square matrix.
##
## Usage:
##  > m = matrix(c(1,2,3,4), nrow=2, ncol=2)
##  > mInv <- cacheSolve(makeCacheMatrix(m))
##  > mInv %*% m  
##  (results in 2x2 Identity matrix)

## Creates a cacheable representation of a matrix object.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL # Inv. matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(m) inv <<- m
        getinverse <- function() inv
        fns <- list(get = get,
                    set = set,
                    getinverse = getinverse,
                    setinverse = setinverse)
        invisible(fns)
}


## Finds the inverse of a square matrix and caches the result.

cacheSolve <- function(x, ...) {

        inv <- x$getinverse()
        
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setinverse(inv)

        inv
}
