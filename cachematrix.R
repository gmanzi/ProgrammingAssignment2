## The following functions are used to cache the inverse of a square matrix.

## makeCacheMatrix contains a function that creates the cache of the inverse 
## of the matrix.

## Assignment #2 Part 1

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
        
}

## cacheSolve contains a function that checks if the inverse of the matrix
## has already been solved. If so, the inverse is retrieved from the cache
## and a message is printed alerting that cached data is being used. If not, 
## the function continues by solving the matrix and prints the result.

## Assignment #2 Part 2

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
