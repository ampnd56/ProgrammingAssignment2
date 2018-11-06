## The following functions take an invertible square matrix and
## cache the inverse of the given matrix.
## @author Amulya Raj Pandey
## @version 5th November, 2018

## This function creates a matrix object which is a list of 
## functions in order to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        matrixInverse <- NULL
        
        set <- function(y){
                
                x <<- y
                matrixInverse <<- NULL
                
        }
        
        setInverse <- function(inverse) matrixInverse <<- inverse
        
        get <- function() x
        getInverse <- function() matrixInverse
        
        list(set = set,
             setInverse = setInverse,
             get = get,
             getInverse = getInverse)
        
}


## This function returns the inverse of the matrix returned
## by the makeCacheMatrix function above. If the inverse has
## already been calculated it just retrieves the data from the
## cache.

cacheSolve <- function(x, ...) {
        
        matrixInverse <- x$getInverse()
        
        if(!is.null(matrixInverse)){
                
                message("getting cached inverse data")
                return (matrixInverse)
                
        }
        
        specialMatrix <- x$get()
        
        matrixInverse <- solve(specialMatrix, ...)
        x$setInverse(matrixInverse)
        
        matrixInverse
}
