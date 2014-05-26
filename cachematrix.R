## The following functions will allow to cache a matrix inverse, so that the next time
## you want the inverse of the same matrix, it doesn't have to be computed again.

## This function creates a special matrix that stores useful information and functions 
## that allow to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        get.inv <- function() inv
        set.inv <- function(inverse) inv <<- inverse
        list(get = get, get.inv = get.inv, set.inv = set.inv)
}


## This function computes the inverse of the special matrix created with the
## 'makeCacheMatrix' function if the inverse has not been computed previously.
## If the inverse of the special matrix has already been computed, this function will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get.inv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set.inv(inv)
        inv
        
        ## Just testing another way to perform the same from line 24
        #if(is.null(inv)){
        #     data <- x$get()
        #     inv <- solve(data, ...)
        #     x$set.inv(inv)
        #     return(inv)
        #} else {
        #     message("getting cached data")
        #     return(inv)
        #}
}
