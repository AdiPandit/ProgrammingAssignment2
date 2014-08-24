## Author : AdiPandit
## File : chachematrix.R
## This file contains implementation of a matrix inverse caching mechanism
## 

## makeCacheMatrix initializes a cache that can store a  matrix and its inverse
## it also defines functions that can be used to get and set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    #initialize the inverse of x to NULL
    ix <- NULL
    
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    
    #return the original matrix
    get <- function() x
    
    #set the inverse of x
    setinverse <- function(inverseofx) ix <<- inverseofx
    
    #get the inverse of x
    getinverse <- function() ix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("returning cached data")
        return(ix)
    }
    #compulte the inverse as we do not have it in cache
    currMatrix <- x$get()
    ix <- solve(currMatrix, ...)
    #store it cache for future use
    x$setinverse(ix)
    #return the inverse
    message("returning computed inverse as we did not have it in cache")
    ix
}
