## The following functions create a matrix-like object that can cache its inverse
## as well as a function that checks to see if the inverse is cached before
## computing it

## Takes an invertible matrix that can be changed, return its value, and caches an inverse
## which can also be changed and returned.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { # allows changing matrix
                x <<- y
                inv <<- NULL # resets inverse if matrix changes
        }
        get <- function() x # returns matrix x
        setinv <- function(inverse) inv <<- inverse ## sets inverse
        getinv <- function() inv # returns inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv) # assigns names to functions
}


## Takes an object created using makeCacheMatrix. If cached inverse exists, returns that
## otherwise it calculates the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() # Pulls cached inverse
        if(!is.null(inv)) { # If inverse was cached already, returns the cached value
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get() # gets value of special matrix object
        inv <- solve(mat) # calculates inverse
        x$setinv(inv) # caches inverse
        return(inv) # returns inverse
}
