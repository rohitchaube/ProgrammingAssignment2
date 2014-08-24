## Prog Assign 2 - R Programming - Aug 2014 
## Desc - A pair of functions that cache the inverse of a matrix 

## Function name - makeCacheMatrix 
## Desc - This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInvMat <- function(solve) invMat <<- solve
        getInvMat <- function() invMat
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## Function name - cacheSolve
## Desc - This function computes the inverse of the special 'matrix' returned 
## by makeCacheMatrix function. If the inverse has already been calculated then 
## this function with retrieve the inverse from the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInvMat()
        if(!is.null(invMat)) {
                message("getting cached Matrix data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setInvMat(invMat)
        invMat
}