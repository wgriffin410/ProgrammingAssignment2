## The functions created in this assignment take advantage of R scoping rules to avoid costly
## repeated computation of inverse matrices by staging to cache the value of the inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
                x <<- y
                imat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) imat <<- inverse
        getInverse <- function() imat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imat <- x$getInverse()
        if(!is.null(imat)) {
              message("getting cached data")
              return(imat)
        }
        mat <- x$get()
        imat <- solve(mat, ...)
        x$setInverse(imat)
        imat
}
