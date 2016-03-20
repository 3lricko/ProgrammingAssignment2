
## This function creates a list of functions to manipulate 
## a matrix, allowing to cache the inverse calculation.


## It returns a list or four functions: set, get,
## setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(invParam) {
        inv <<- invParam
    }
    getInverse <- function(){
        inv
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Using the list created by makeCacheMatrix(), this method calculates
## the inverse of a matrix, and caches the result to avoid caculating it
## every time (if the matrix hasn't changed)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    
    inv
}
