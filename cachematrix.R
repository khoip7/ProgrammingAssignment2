
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInvMatrix <- function(solve) inv <<- solve
        getInvMatrix <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInvMatrix()
        if(!is.null(inv)) {
                message("retrieving the inverse from the cache")
                return(inv)
        }
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInvMatrix(inv)
        inv
}
