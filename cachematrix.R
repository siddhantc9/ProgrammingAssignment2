## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions cache the inverse of a matrix.

## The first function makeCacheMatrix
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # @x is sqaure invertible matrix
        #              1. set the matrix
        #              2. get the matrix
        #              3. set the inverse
        #              4. get the inverse
        #         this list is used as the input to cacheSolve()
        
        # @inv is inverse matrix which is null first time
        inv <- NULL
        
        # set the value of the matrix 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of inverse matrix
        setInverseMatrix <- function(inverse) inv <<- inverse
        
        # get the value of inverse matrix
        getInverseMatrix <- function() inv
        
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        # @x: output of makeCacheMatrix()
        # return: inverse of the original matrix which was input to makeCacheMatrix()
        
        inv <- x$getInverseMatrix()
        
        # check if the inverse has already been calculated
        if(!is.null(inv)) {
                # get it from the cache and skip the computation
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        data <- x$get()
        inv <- solve(data, ...)
        
        # set the value of the inverse in the cache using the setinv function
        x$setInverseMatrix(inv)
        
        inv
}
