###############################################################################
## SCRIPT cachematrix.R
## There are two functions in this script:
## 1. makeCacheMatrix: This function creates a special object that can cache
##    a matrix its inverse, and returns a list of get and set functions.
## 2. cacheSolve: This function retrieves the inverse of the above mentioned 
##    matrix from cache if available, or computes it , and returns it.
##    Throws error if the input matrix is not invertible.
## NOTE: See section SAMPLE RUN at the end of the script for test outputs.
###############################################################################

## FUNCTION makeCacheMatrix (matrix = matrix())
## Creates a special object that can cache a matrix and its inverse, and  
## returns a list of get and set functions for the matrix and its inverse.
## matrix - holds the input matrix in cache
## inverse - holds the inverse of the matrix in cache
## set() - sets the value of matrix and caches it
## get() - returns the matrix, held by matrix
## setInverse() - sets the value of inverse and caches it
## getInverse () - returns the inverse, held by inverse

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(input) {
        matrix <<- input     ## cache the input matrix
        inverse <<- NULL     ## clear inverse from cache when matrix changes
    }
    get <- function() {
        matrix
    }
    setInverse <- function(inv) {
        inverse <<- inv                  ## cache the inverse
    }       
    getInverse <- function() {
        inverse
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)        ## return list of all functions
}

## FUNCTION cacheSolve(cacheMatrix)
## Retrieves and returns the inverse of the matrix cached by makeCacheMatrix,
## if the inverse has already been computed and the matrix has not changed. If
## not, computes the inverse newly and returns it.
## Throws error while calling solve() if the input matrix is not invertible.

cacheSolve <- function(cacheMatrix) {
    inverse <- cacheMatrix$getInverse()        ## get the inverse from cache...
    if(!is.null(inverse)) {                    ## and if it is not null...
        message("getting cached data")         ## show a message, and...
        return(inverse)                        ## return it
    }
    matrix <- cacheMatrix$get()                ## if not, take the matrix...
    inverse <- solve(matrix)                   ## and compute its inverse...
    cacheMatrix$setInverse(inverse)            ## set it in cache, and...
    inverse                                    ## return it
}

###############################################################################
## SAMPLE RUN
## > source("cachematrix.R")
## > mcm <- makeCacheMatrix() # create the special object using makeCacheMatrix
## > mcm$set(matrix(c(2,2,3,2),2,2)) # set some matrix in it
## > mcm$get() # let us see if it got set correctly
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > cacheSolve(mcm) # run cacheSolve to get its inverse
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > mcm$getInverse() # see if the inverse got set correctly
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(mcm) # run cacheSolve again to see if we are getting 
##                   # the inverse from cache
## getting cached data
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > mcm$set(mcm$getInverse()) # force a change in the matrix
##                             # let us use the inverse for it
## > mcm$get() # run get to see if the inverse got set to matrix
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > mcm$getInverse() # run getInverse and see if the cache got cleared
## NULL
## > cacheSolve(mcm) # run cacheSolve again, we should get our original matrix
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > mcm$getInverse() # finally, let us run getInverse again
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
###############################################################################