## These functions create a matrix object that can cache its inverse.
## The inverse of the matrix is stored after the first computation so 
## that repeated requests do not require recomputation.

## This function creates a matrix object to store a matrix and cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } 
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of makeCacheMatrix. If the 
## inverse has already been calculated, it retrieves the cached 
## inverse instead of recomputing.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
