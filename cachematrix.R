## Functions to cache and return inverse of a vector. If the functions have been called on the same matrix before,
## it returns the cached inverse to save computation time.

## Function to return a parsed matrix along with helper functions for retrieving cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinv <- function(inv) I <<- inv
    getinv <- function() I
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns inverse of matrix from above function either from cache or by calculation if no cached inverse available.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinv()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinv(I)
    I
}
