## Put comments here that give an overall description of what your
## functions do



# Create a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse value
    i <- NULL
    # set  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get value
    get <- function() x
    # set mean
    setinverse <- function(inverse) i <<- inverse
    # get mean
    getinverse <- function() i
    # return
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Computes the inverse of the previous matrix

cacheSolve <- function(x, ...) {
    # get the value of the 'getmean' slot
    i <- x$getinverse()
    # si no es nulo devuelve lo que encontro
    if (!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    # si es nulo extrae el objeto
    data <- x$get()
    # calcula su inversa
    i <- solve(data, ...)
    # lo guarda en el objecto original
    x$setinverse(i)
    # return computed inverse
    i
}
