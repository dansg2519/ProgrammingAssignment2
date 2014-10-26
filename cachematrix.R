## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
	
    set <- function( matrix ) {
        x <<- matrix
        i <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inverse) {
        i <<- inverse
    }

    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## Calculate the inverse of the matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
	
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
