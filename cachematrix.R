## Functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    mxInv <- NULL
    set <- function(y) {
        x <<- y
        mxInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) mxInv <<- inv
    getInv <- function() mxInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getInv()
    # check cache first
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    # calculate inverse
    data <- x$get()
    mx <- solve(data, ...)
    # store in the cache
    x$setInv(mx)
    mx
}
