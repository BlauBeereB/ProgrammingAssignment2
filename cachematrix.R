## Pair of functions that invert a given matrix and 
## cache the values of the matrix and the inverse.
## In the case that the matrix is unchanged since the 
## last calculation, the cached inverse is returned.

## The following function takes a matrix argument and 
## returns a "matrix object" that can get and set the value 
## of the matrix as well as the value of its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    # Check if given matrix is square:
    if (nrow(mat) != ncol(mat)) {
        message("Given matrix must be square.")
        return()
    }
    
    # Define function to set the value of the matrix:
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    
    # Define function to get the value of the matrix:
    get <- function() mat
    
    # Define function to set the inverse of the matrix:
    setinv <- function(inverse) inv <<- inverse
    
    # Define function to get the inverse of the matrix:
    getinv <- function() inv
    
    # Collect functions into a list:
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}

## The following function takes a "matrix object" (generated 
## by the function makeCacheMatrix) and returns its inverse. 
## In case of an unchanged matrix, the function returns a 
## cached value of the inverse instead of calculating it again.

cacheSolve <- function(matlist, ...) {
    # Check if matrix inverse has been calculated:
    inv <- matlist$getinv()
    if (!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    
    # If not, get matrix data and calculate inverse:
    data <- matlist$get()
    inv <- solve(data, ...)
    matlist$setinv(inv)
    inv
}
