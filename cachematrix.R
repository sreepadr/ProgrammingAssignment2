############################################################################### 
## SCRIPT cachematrix.R
##
## There are two functions in this script:
## 1. makeCacheMatrix: This function creates a special "matrix" object that 
##    can cache its inverse, and retruns a list of "get" and "set" functions.
## 2. cacheSolve: This function computes the inverse of the above "matrix" 
##    or retrieves it from cache if available.
##
## NOTE: See section SAMPLE RUNS at the end of the script for test outputs.
###############################################################################

## FUNCTION makeCacheMatrix (x = matrix())
## Creates a special "matrix" object that can cache its inverse, and retruns 
## a list of "get" and "set" functions for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## FUNCTION cacheSolve(x, ...)
## Computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    
}

###############################################################################
## SAMPLE RUN
## > source("cachematrix.R")
## > mcm <- makeCacheMatrix() # create a special matrix using makeCacheMatrix
## > mcm$set(matrix(c(2,2,3,2),2,2)) # set some data in it
## > mcm$get() # let us see if it got set correctly
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > cacheSolve(mcm) # run cacheSolve to get its inverse
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > mcm$getInverse() # see if the inverse got set in the special matrix
##                    # and we can retrive it using getInverse
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > mcm$set(mcm$getInverse()) # force a change in the data
##                             # let us pass the inverse for it
## > mcm$getInverse() # run getInverse and see if the cache got cleared
## NULL
## > cacheSolve(mcm) # run cacheSolve again, we should get our original matrix
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > mcm$getInverse() # finally, run getInverse again
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## >
###############################################################################