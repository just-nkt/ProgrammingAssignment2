## Accepting matrix as an input and computing inverse of a matrix.
## Storing the inverse of a matrix in cache

## Caching the original matrix, creating getters and setters for a matrix
## and for the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        s = NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## Retrieving the inverse of a matrix from cache. If it does not exist,
## calculating and storing in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("retrieving inverse matrix from cache")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}