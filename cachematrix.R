#---------------------------------------------------------------------------
# Author: Jacob Townson
# Date : 06/11/2015
# Title: Project 2
# Class: R Programming
# These functions take input of a matrix to find the inverse of the matrix. 
# Once calculated, the inverse is cached for future work.
#---------------------------------------------------------------------------

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. Use this functions output to put into the 
## cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m        
}
