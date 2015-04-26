##
# function makeCacheMatrix (matrix): 
#       Creates a cahce object to stroe a 
#       a matrix along with its inverse and provides methods/functions
#       to manage them
#
# function cacheSolve (cachedMatrixObject):
#       Used to compute the inverse of the matrix cached and store it in the
#       cache for re-use.
##

# makeCacheMatrix - creates a matrix cache object with methods/functions to 
# set and get matrix objects with their inverses
# Methods / Functions for this object are as follows:
# 1. get: gets the matrix that is cached
# 2. set: sets the matrix to be cached
# 3. getInverse: gets the inverse of the matrix that is cached
# 4. setInverse: sets the inverse of the matrix that is cached. 
#
#  Usage:
#  cachedMatrixObject <- makeCacheMatrix (matrixToCache)
#  cachedMatrix <- cachedMatrixObject$get()
#  inverseOfCacheMatrix <- cachedMatrixObject$getInverse()
#  cachedMatrixObject$set (matrixToCache) - sets or replaces an existing cached matrix with 
#                                           matrixToCache
#  cachedMatrixObject$setInverse (inverseOfCachedMatrix) - sets the inverse of cachedMatrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (m) {
        y <<- m;
        inv <<- NULL;
    }
    
    get <- function() x
    
    setInverse <- function (minv) {
        inv <<- minv
    }
    
    getInverse <- function () inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve:
# Takes the cached matrix object returned from makeCacheMatrix() call
# and computes the inverse of the matrix stored in the cache.
# If the cache already has the inverse computed returns that otherwise returns
# the computed inverse of the matrix
# Before computing the inverse, checks if the matrix is square or not. If not square
# returns NULL.

# Usage:
#   cacheSolve (cachedMatrixObject)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Checks if the inverse is already computed and returns the computed one
    # else computes one and returns
    
    inv <- x$getInverse()
    if (! is.null(inv)) {
        message ("Getting cached data")
        return (inv)
    }
    
    # get the matrix data set
    data <- x$get()
    
    # Check if matrix is square or not
    if (nrow(data) == ncol(data) ) {
        # compute inverse
        inv <- solve(data)
    } else {
        message ("Cannot compute inverse. Matrix not square")
        inv <- NULL
    }
    
    # set the inverse for the matrix
    x$setInverse(inv)
    inv
}

