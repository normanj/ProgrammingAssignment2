## Put comments here that give an overall description of what your
## functions do
##
############################################################################
##
##  The two functions makeCacheMatrix() and cacheSolve() operate collaboratively
##  to provide the inverse of a square matrix.  The inverse is retained so that
##  if the inverse of the same matrix is required again it can be provided
##  without recalculation, provided that another matrix has not been supplied
##  in the mean time.
##
##  The two functions work together as follows:
##
##    -  makeCacheMatrix() receives the input matrix and sets the inverse to
##       NULL, indicating that the inverse is not known.
##
##	  -  makeCacheMatrix() also defines a set of helper function which are
##       returned as a list. These functions enable the input matrix to be
##       retrieved and it's inverse to be set and subsequently retrieved.
##       This inverse is retained until a new matrix is supplied.  In this
##       event the inverse is again set to NULL indicating to cacheSolve()
##       that a new calculation of the inverse is required (for the new matrix).
##
##	  -  cacheSolve() is called with the helper function list previously
##       returned by makeCacheMatrix() as an input paramter.
##
##	  -  Helper function getinv() returns the matrix inverse.  If the inverse
##       is returned as NULL, then makeCacheMatrix() uses the get() helper
##       function to retrieve the matrix and uses solve() to calculate its
##       inverse.   The inverse is the return value of cacheSolve() and is also
##       returned to makeCacheMatrix() using setinv() for potential re-use.
##
##	  -  If the inverse returned by getinv() is not null, then the inverse for
##       the matrix in question has already been calculated, and is returned
##       to the caller without further processing.
##
##       Author:   Norman Jessup
##       Date:     25 Sep 2015
##
###############################################################################
## Write a short comment describing this function.
##
##  makeCacheMatrix() accepts and holds a matrix and sets the inverse to NULL.
##  when the inverse is calculated makeCacheMatrix() retains it for possible
##  re-use. It   also defines the helper functions:
##
##    - set() Available to save the input matrix, but not used.
##    - get()  Retrieves the current matrix for inversion
##    - setinv() Saves the inverse for potential re-use
##    - getinv() Retrieves the inverse
##
################################################################################
##
##  NOTE:  In common with the example program (makeVector), the matrix inverse
##         is calculated whenever a matrix is supplied using makeCacheMatrix(),
##         even if the matrix supplied is identical to the previous one.
##
##         Depending on the expected usage, and the size of matrices to be used,
##         an enhancement to test that the matrix is actually different from
##         used previously could be a worthwhile enhancement.  This depends
##         on how the module is used.
##
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # set() function saves the input matrix and resets the inverse.
    # Note that set is included to match the example code, but is not
    # actually used.
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get() function returns the last input matrix
    get <- function() x
    
    
    # setinv() saves the inverse in the calling function (makeCacheMatrix)
    # getinv() retrieves it
    setinv <- function(inverse) inverse <<- inverse
    getinv <- function() inverse
    
    # Return the functions set(), get(), setinv() and getinv() as a list
    # This enables these functions to be accessed outside of
    # makeCacheMatrix()
    
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

#############################################################################
##
## Write a short comment describing this function
##
## cacheSolve() provides the matrix inverse.  If this has already been
## computed and cached then the inverse is retrieved from the cache.
## Otherwise, the matrix is retrieved using the x$get() function and the
## inverse is calculated using solve(). The inverse is the
## return value of the function and, if it has been calculated, it is saved
## using x$setinv() so it can be re-used.
##
## The input argument, x, is the list of the 4 matrix functions that was
## returned by makeCacheMatrix().
##
############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # See if the inverse already exists
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("inverse retrieved from cache")
        return(inverse)
    }
    
    # No inverse in the cache - need to calculate it
    data <- x$get()
    inverse <- solve(data)
    
    # save the calculated inverse for re-use
    x$setinv(inverse)
    return(inverse)
}

############################################################################

