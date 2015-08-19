## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The functin is basically the same as the makeVectorfunction, except the class is changed to matrix and the variablenames.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(nrow(data) != ncol(data) || det(data) == 0 ) {
        return(message("Matrix not invertible."))
    }
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## I added an if-statement to check whether a Matrix is invertible (line 31-33)

