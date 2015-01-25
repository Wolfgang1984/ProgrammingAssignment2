## Put comments here that give an overall description of what your
## functions do

## Constructor functions that has all the functions related to create the inverse of a square invertable matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv() #gets the inverted matrix currently on cache
        if(!is.null(m)) { # if the value is different than null recovers the matrix from cache
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if the value is  null calculates and saves the inverted matrix in cache
        m <- solve(data, ...)
        x$setinv(m)
        m
}
