## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it
## repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). Your assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
'''
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
'''
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## 1. set the value of the matrix
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        ## 2. get the value of the matrix
        get <- function() x
        
        ## 3. set the value of the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## 4. get the value of the inverse matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
'''
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
'''
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## Determine whether the inverse matrix has been calculated and stored
        if (!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## 'm' is the inverse matrix of 'x'
        m
}
