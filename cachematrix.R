## cachematrix.R
##
## These functions implement a CacheMatrix, a matrix that
## caches its inverse matrix to speed up computations.
## makeCacheMatrix(x) constructs a CacheMatrix from a matrix.
## solveCache(x) solves x for its inverse.


## makeCacheMatrix is a constructor for a matrix-like object
## that caches its inverse. It takes a matrix as argument.
##
## The CacheMatrix has four methods:
## * `get` gets the value of the matrix
## * `set` sets the value of the matrix
## * `getinv` gets the inverse matrix
## * `setinv` sets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set  <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## cacheSolve solves the CacheMatrix inverse, either
## by returning a cached value or computing the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
