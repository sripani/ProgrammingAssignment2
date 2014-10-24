## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
        
        ## Initializing value of the inverse property
        m <- NULL
        
        ## Set the matrix
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                ## Return the matrix
                x
        }
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse
        }
        
        ## get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                m
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}