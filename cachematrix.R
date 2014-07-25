## Defines as special "matrix" object and associated function to calculate
## and cache the matrix inversion. 


## Creates a special "matrix" object that can cache its inverse. 
## 'x' is a square invertible matrix

## Return a list containing a function to
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse of the matrix
##   4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculate the inverse of the special matrix object.  If the inverse has 
## already been clalculated and unchanged then the cashed inverse is returned
## 'x' is a special matrix object created from makeCacheMatrix

## Return is a matirx that is the inveraw of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
