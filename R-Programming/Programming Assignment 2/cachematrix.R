## makeCacheMatrix takes a matrix as an argument and creates a list that allows
## the user to (1) get/set the value of a matrix and (2) get/set the value
## of that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    
    matrixInverse <- NULL
    
    ##Retrieve the matrix
    getMatrix <- function() x
    
    ##Take in a new matrix, reset the inverse to null
    setMatrix <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    ##Retrieve cached solved matrix
    getInverse <- function() matrixInverse
    
    ##Sets a new inverse
    setInverse <- function(inverse){
        matrixInverse <<- inverse
    }
    ## Returns a list of functions
    list(get = getMatrix,
         set = setMatrix,
         getInverse = getInverse,
         setInverse = setInverse)
}

## cacheSolve returns the inverse of a square, invertible matrix. It checks to
## see if a solution has already been cached, in which case the previously
## computed inverse is returned. If not, it uses the solve() function
## to create a new inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getInverse()
    
    ##If cached data exists, return it
    if(!is.null(matrixInverse)){
        message("Getting cached data...")
        return(matrixInverse)
    }
    
    ## If matrixInverse was null, compute a new inverse using solve(), then
    ## store the computation with setInverse()
    data <- x$get()
    matrixInverse <- solve(data)
    x$setInverse(matrixInverse)
    
    ## Print the result
    matrixInverse
}