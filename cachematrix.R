## Description of functions:
## makeCacheMatrix creates a list a functions for caching the inverse of a matrix
## cacheSolve returns the inverse of the matrix stored in makeCacheMatrix

## The following function creates a matrix, and the functions for getting/settings the matrix and its inverse
## The function returns the list of functions for the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(newinv) inv <<- newinv
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function returns the inverse of the matrix in makeCacheMatrix
## If possible, this function will return a cached version of the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
