## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse. This function is really
## a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL
    set.mat <- function(y) {
        x <<- y
        inv.mat <<- NULL
        }
    get.mat <- function() x
    set.inverse <- function(solve) inv.mat <<- solve
    get.inverse <- function() inv.mat
    list(set.mat=set.mat, get.mat=get.mat, set.inverse=set.inverse, get.inverse=get.inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv.mat <- x$get.inverse()
    if(!is.null(inv.mat)) {
        message("Getting cached inverse matrix")
        return(inv.mat)
        }
    data <- x$get.mat()
    inv.mat <- solve(data)
    x$set.inverse(inv.mat)
    inv.mat
}
