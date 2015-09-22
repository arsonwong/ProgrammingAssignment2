## The makeCacheMatrix creates a special "matrix" which is a list of 
## functions to set/get the value of the matrix, and set/get the value of
## its inverse.  The cacheSolve function takes such a special "matrix" as
## argument, and returns its inverse, either by returning the cached value
## if the matrix has not changed and its inverse has been previously computed
## by calling cacheSolve, or otherwise by calculation by calling solve.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

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


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data by calling solve and sets the inverse matrix
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
