## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a matrix, x, and sets or returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(suppliedValue = matrix()) {
        x <<- suppliedValue 
        cachedInverse <<- NULL
        }
    get <- function() x
    setInverse <- function(inverseValue) {
        cachedInverse <<- inverseValue 
        return(cachedInverse)
        }
    getInverse  <- function() cachedInverse {
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        }


## cacheSolve computes the inverse of the matrix created with makeCacheMatrix.
## If the inverse has already been calculated and the matrix hasn't changed then
## the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
    solvedInverse <- x$getInverse()
    if(!is.null(solvedInverse)){
        message("getting cached data")
        return(solvedInverse)
        }
    matrix<-x$get()
    solvedInverse<-solve(matrix, ...)
    x$setmatrix(solvedInverse)
    solvedInverse
    }
