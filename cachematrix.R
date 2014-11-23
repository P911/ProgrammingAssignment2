## Two "functions" are provided to implement a
## very specialized cache for the inverse
## of matrices.

## The "function" makeCacheMatrix returns a new
## object given a matrix as parameter.
## The returned object has methods to manipulate
## the object:
## - get/set the matrix
## - getinverse/setinverse to set a second matrix
##   which is supposed to the the inverse of the
##   the first matrix

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
                # the "<<-" operate will modify
                # the variable in the parent environment
                # which is "makeCacheMatrix"
                x <<- y
                imat <<- NULL
        }
        get <- function() x
        setinverse <- function(inversem) imat <<- inversem
        getinverse <- function() imat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function will return the inverse
## matrix of the matrix x.
## As a side effect, the function will cache the
## inverse matrix to accelerate further calls to
## the function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached version of inverse matrix")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
