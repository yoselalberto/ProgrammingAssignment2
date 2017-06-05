## Functions to create, and compute the inverse of an invertible matrix; returning
## the cached value if the matrix have not changed



# Create an object containing a matrix, and slots to it inverse

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


# Computes the inverse of the previous matrix, and asign it to an slot; 
# return cached value, if it exist 

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
