## This fuction will will create a vector that contains both
## a matrix and if it has been computed it's inverse. This vector
## also provided the ability to set and retrive both the matrix and
## the   inverse value.
##
## 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse  <- function(solve) m<<-solve
    getInverse  <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function will take a vector produced usig the makeCacheMatrix
## function and will either return the cached inverse matrix or if the 
## inverse matrix has not been cached it will compute the inverse matrix,
## cache the computed inverse matrix in the vector and will return the invers
## matrix that was computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m  <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}
