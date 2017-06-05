## Functions to create, and compute the inverse of an invertible matrix; returning
## the cached value if the matrix have not changed



# Create an object containing a matrix, and slots for functions to retrieve the object,
# set, and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
    # set inverse value to NULL
    i <- NULL
    # set matrix value
    set <- function(y) {
        # assign matrix
        x <<- y
        # restart inverse value
        i <<- NULL
    }
    # get matrix
    get <- function() x
    # set inverse matrix
    setinverse <- function(inverse) i <<- inverse
    # get inverse matrix
    getinverse <- function() i
    # return
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# First, it checks if do there exist a cached inverse, if it is one, return that value;
# otherwise, compute the inverse, and assign caches that value in the original object

cacheSolve <- function(x, ...) {
    # get the value of the 'get inverse' slot
    i <- x$getinverse()
    # if it exist, return that value with a message
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    ## if it doesn't exist, compute the inverse matrix
    # retrieve matrix
    data <- x$get()
    # compute inverse
    i <- solve(data, ...)
    # assign inverse 
    x$setinverse(i)
    # return computed inverse
    i
}
