## The function makeCacheMatrix gets a matrix as patameter
## and creates special Matrix stored in cache memory
## An example of how to call this function to create the matrix:
## x<-makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))

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


## The function cacheSolve calculates the inverse matrix
## or calls it from caché memory when it's already been calculated
## and stored by the function makeCacheMatrix.
## To continue with the above example, a way to call this function: 
## cacheSolve(x) (x previously created with the function makeCacheMatrix)

## If you call this function twice with the same argument, in the
## second execution you'll see a message: getting cached data.

## Remember x must be a square matrix to calculate its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
