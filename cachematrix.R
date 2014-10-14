## The following two functions combine to avoid repeatedly 
## calculating the inverse of a matrix by caching it.

## This function creates a special "matrix" that is really a list
## containing 4 functions:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix inverse
##   4. get the value of the matrix inverse
makeCacheMatrix <- function(mtx = matrix()) {
    # Initialize the inverse
    inv <- NULL
    
    # Set the matrix and re-initialize the inverse 
    set <- function(m) {
        # Only when the new matrix is different from the current one
        if (!all(mtx == m)) {
            mtx <<- m
            inv <<- NULL
        }
    }
    
    # get the matrix
    get <- function() mtx
    
    # set the inverse
    setInverse <- function(i) inv <<- i
    
    # get the inverse
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of a special "matrix" created
## by the makeCacheMatrix function above. The calculation gets
## skipped if the inverse has been calculated and cached. Otherwise, 
## this function performs the inversion and cache the inverse.
cacheSolve <- function(mtx, ...) {
    # Extract the inverse
    inv <- mtx$getInverse()
    
    # Return the inverse if it has been cached
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Otherwise, calculate, cache and return the inverse
    i = solve(mtx$get(), ...)
    mtx$setInverse(i)
    i
}
