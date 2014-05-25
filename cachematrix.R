## makeCacheMatrix creates a matrix and caches it.  
##cacheSolve computes, caches and returns the inverse matrix.


## This funcion creates x, a matrix, then stores the values of the matrix to call later.
## It also stores the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
    

## This function calls the cached matrix if there is one and if there isn't a cached matrix 
## then it will create an inverse matrix and cache it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
