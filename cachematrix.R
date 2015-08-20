## Put comments here that give an overall description of what your
## functions do
##
##
## In order to mitigate the time it take to invert a matrix multiple times, a
## local vector is used to store the value of a cached verstion of that inversion
## so that it can be used instead of recomputed.
##
##
## These two function are codependent.
##
##     makeCacheMatrix creates the setup for the name and values of
##      functions that retrieve data from a local variable (invertedMatrix)
##      
##      The makeCacheMatrix routine must be called before the cacheSolve routine
##      so that it can create an object with the values of the various
##      functions that operate on the cache.
##
##      cacheSolve uses the solve function to invert a matrix.  
##      its input is an object that has the defined matrix plus functions that
##      will work on the cache (a one value cache)
##
##      It will call solve for the matrix inversion and set the cache.
##
##
## Usage:
##      myMatrix <- makeCacheMatrix(originalMatrix)
##      myInversion <- cacheSolve(myMatrix)
##
## Write a short comment describing this function
## 
## This function accepts a matrix as input.  
## It creates a 'global' variable (invertedMatrix) 
## and defines it as initially NULL.  It defines other sub functions that 
## will set[invert] or get[invert] that use that global variable as 
## the cache indicator. It will also store the original matrix.
##
## The routine will also define routines that can be used to either set the
## value or get the value of this inverted matrix.
##
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) im <<- solve
        getinvert <- function() im
        list (set = set, get = get,
              setinvert = setinvert, getinvert = getinvert)
}


## Write a short comment describing this function
##
## This routine determines if there is already a cached inversion
## of our matrix.  If there is, it returns that value and a message.
##
## The input to this routine is not a matrix, but an object created
## makeCacheMatrix that has the value of the matrix and functions to operate on
## that object.
##
## if there is no inversion previous value, we create a new inversion matrix, 
## call a routine to set the cache value and return the new inversion
## value.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        im <- x$getinvert()
        if (!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvert(im)
        im
}
