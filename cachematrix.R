## This file contains two functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
## Call makeCacheMarix to initialise a matrix that can be inverted
## e.g MatrixA <- matrix(c(5,1,0,3,-1,2,4,0,-2),nrow=3,ncol=3)
## ourMatrix = makeCacheMatrix(MatrixA)
## 
## Subsequently call cacheSolve to calculate or retrieve the inverse of the matrix
## e.g cacheSolve(ourMatrix)
## Call the function again to check the inverse is being obtained from the cache.
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        data <- x$get()
        if(!is.null(inv))  {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
