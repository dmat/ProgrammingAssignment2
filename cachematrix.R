## Provides functions to find inverse of a matrix. Finding inverse
## of a matrix is expensive. This optimized function computes the 
## inverse and cache the result for subsequent calls. 

## This function crates a special matrix object that can
## can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    ## variable to keep the inverse and initialized to NULL
    i <- NULL
    ## function to cache matrix 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## function to return matrix
    get <- function() x
    ## function to cache inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    ## function to return the inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## This function computes the inverse of special matrix created by 
## makeCacheMatrix above. If inverse is already created, this function 
## returns the inverse from cahce.
## Return a matrix that is the inverse of 'x'. This method assumes
## matrix 'x' is squre invertible, otherwise throws error.
cacheSolve <- function(x, ...) {
    ## First check if inverse of x is already computed
    i <- x$getinverse()
    if(!is.null(i)){
        ## Inverse of x is already compted. So, show a message
        ## and return inverse from cache 
        message("getting cached data")
        return(i)
    }
    ## Inverse is not in cache. Get the matrix to compute inverse
    data <- x$get()
    ## FInd the inverse by calling solve()
    ## Solve computes inverse only for squre invertible matrix
    i <- solve(data, ...)
    ## Cache the result
    x$setinverse(i)
    ## return the inverse
    i
}
