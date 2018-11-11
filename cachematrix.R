## The following functions create a special kind of matrix
## that enables R to cache the result of a matrix inversion (Which is computationaly expensive)

## This function gets a matrix as an input and returns a list of functions 
## that handle the matrix values and the inverted matrix values by using lexical scoping

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function receives a object created using makeCachaMatrix( x = matrix() )
## and checks wether an inversion on that matrix occured previously.
## If it was calculated before, it returns the cached value; otherwise it performs the calculation, caches it and returns it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
