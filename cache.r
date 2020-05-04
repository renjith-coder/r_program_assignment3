## Put comments here that give an overall description of what your functions do

# 'makeCacheMatrix' create a matrix with added functions to keep a cache of the matrix's inverse
# 'cacheSolve' returns the inverse of a matrix created with 'makeCacheMatrix', either by 
#       calculating the inverse, or by returning the cached value

## Write a short comment describing this function

# Create a special "matrix" object that can cache its inverse.
# provides:     set (m) : sets the matrix to be operated on 
#               get (m) : gets the matrix
#               setInverse (mi) : sets the inverse of the matrix
#               getInverse (mi) : gets the inverse of the matrix
                
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(newX) {
                x <<- newX
                cachedInverse <<- NULL
        }
        get <- function() {return (x)}
        setInverse <- function(newInverse) {cachedInverse <<- newInverse}
        getInverse <- function() {return (cachedInverse)}
        return (list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}

## Write a short comment describing this function

# Compute the inverse of the special "matrix" returned by `makeCacheMatrix` 
#       above. If the inverse has already been calculated (and the matrix has not changed), 
#       then retrieve the inverse from the cache. 
## Return a matrix that is the inverse of 'cm'
#  Requires a matrix created via makeCacheMatrix
cacheSolve <- function(cm, ...) {
        
        cachedInverse <- cm$getInverse()
        if(!is.null(cachedInverse)) {
                message("getting cached data")
                return(cachedInverse)
        }
        data <- cm$get()
        inverse <- solve(data, ...)
        cm$setInverse(inverse)
        return (inverse)
}

## Not really meant to be executed, but a good place to cache test data to cut and paste
testIt <- function () {
        m1 <- matrix (c(2, 4, -3, -7), nrow=2)
        m2 <- matrix (c(2, 4, 6, -4, -5, -8, 7, 4, 5), nrow = 3)
        cm1 <- makeCacheMatrix (m1)
        cm2 <- makeCacheMatrix (m2)
        cacheSolve (cm1)
        cacheSolve (cm2)
        cm1$set (m2)
        cacheSolve (cm1)
        cacheSolve (cm1)
}