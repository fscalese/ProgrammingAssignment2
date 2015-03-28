## The function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the values of the matrix
## get the values of the matrix
## set the values of the inverse matrix
## get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) mInverse <<- Inverse
        getInverse <- function() mInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The function cacheSolve calculates the inverse matrix of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the inverse matrix
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                message("getting cached data")
                return(mInverse)
        }
        matrix <- x$get()
        mInverse <- solve(matrix, ...)
        x$setInverse(mInverse)
        mInverse
        ## Return a matrix that is the inverse of 'x'
}
