# The whole description of the concept of a CacheMatrix and the desired output is described in the assignment
# or in README.md in the assignment's repository.
# The following functions have been thoroughly tested via unit tests and the caching mechanism works as expected.

## makeCacheMatrix function creates a special "matrix" object that caches its inverse. It is described by
## returning a list of functions to handle the following operations:
## (1) set the value of a matrix
## (2) get the value of the matrix
## (3) set (i.e. cache) the inverse matrix
## (4) get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        cachedInvMatrix <- NULL
        
        
        # (1)
        setMatrix <- function(y) {
                x <<- y
                cachedInvMatrix <<- NULL
        }
        
        # (2)
        getMatrix <- function()
                x
        
        # (3)
        setInvMatrix <- function(invMatrix)
                cachedInvMatrix <<- invMatrix
        
        # (4)
        getInvMatrix <- function()
                cachedInvMatrix
        
        # CacheMatrix's list of functions, i.e. the "special" matrix object
        list(
                setMatrix = setMatrix,
                getMatrix = getMatrix,
                setInvMatrix = setInvMatrix,
                getInvMatrix = getInvMatrix
        )
}


## cacheSolve calculates the inverse matrix, if not yet cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get cached inverse matrix
        invMatrix <- x$getInvMatrix()
        
        # if inverse matrix was already cached, go into IF statement
        if (!is.null(invMatrix)) {
                message("getting cached matrix")
                # return the inverse
                return(invMatrix)
        }
        
        # otherwise, get the matrix
        matrix <- x$getMatrix()
        
        # invert the matrix
        inv <- solve(matrix, ...)
        
        # set and cache the inverse matrix in the "special" matrix object
        x$setInvMatrix(inv)
        
        # return the inverse
        inv
}






