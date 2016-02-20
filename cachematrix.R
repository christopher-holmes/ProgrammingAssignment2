## makeCacheMatrix - Takes a matrix as an input and contains functions to hold and manage the values of the matrix and the inverse of the matrix

##setMatrix - Sets the value of the user supplied matrix

##getMatrix - Returns the value of the matrix that the user supplied

##setInverse - Sets the value of the matrix per the value returned externally (from cacheSolve in this case)

##getInverse - Returns the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) matrixInverse <<- solve
        getInverse <- function() matrixInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve - makes calls against makeCacheMatrix functions and then solves for a matrix inverse if needed and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached data")
                return(matrixInverse)
        }
        data <- x$getMatrix()
        matrixInverse <- solve(data, ...)
        x$setInverse(matrixInverse)
        matrixInverse
}