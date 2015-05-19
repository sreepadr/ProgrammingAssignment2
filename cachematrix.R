############################################################################### 
## SCRIPT cachematrix.R
## 
## There are two functions in this script:
## 1. makeCacheMatrix: This function creates a special "matrix" object that 
##    can cache its inverse, and returns a list of "get" and "set" functions.
## 2. cacheSolve: This function computes the inverse of the above "matrix" 
##    or retrieves it from cache if available.
##
## NOTE: See section SAMPLE RUNS at the end of the script for test outputs.
###############################################################################

## FUNCTION makeCacheMatrix (x = matrix())
## Creates a special "matrix" object that can cache its inverse, and  
## returns a list of "get" and "set" functions for the matrix and its inverse.
## x - holds the input matrix
## inv - holds the inverse of the matrix
## set() - sets the input matrix to x and caches it
## get() - returns the matrix held by x
## setInverse() - sets the inverse of matrix held by x to inv and caches it
## getInverse () - returns the inverse, held by inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y            ## cache the input matrix
        inv <<- NULL       ## clear the inverse from cache when matrix changes
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) ## return list of all get and set functions
}

## FUNCTION cacheSolve(x, ...)
## Computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been computed (and the matrix has not changed),
## then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()               ## get the inverse from cache...
    if(!is.null(inv)) {                 ## and if it is not null...
        message("getting cached data")  ## show a message, and...
        return(inv)                     ## return it
    }
    data <- x$get()                     ## if not, take the matrix...
    inv <- solve(data, ...)             ## and compute its inverse...
    x$setInverse(inv)                   ## set it in cache, and...
    inv                                 ## return it
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