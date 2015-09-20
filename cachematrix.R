## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that returns a list of functions
# It stores a matrix and the cached value of the inverse of the 
# matrix. It consists of the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * setInverse     set the cached value (inverse of the matrix)
# * getInverse     get the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
        # holds the cached value
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # stores a matrix
        setMatrix <- function(y) {
                x <<- y
                # since the matrix is assigned a new value, cache is NULL
                cache <<- NULL
        }
        # returns the stored matrix
        getMatrix <- function()x

        # cache the given argument 
        setInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function()cache
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)


}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix.However, it first checks to see if the inverse has already been calculated
# If so, it gets the inverse from the cache and skips the computation
# Otherwise,it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        # get the cached value
        inverse <- x$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("Inverse already exists.Fetching cached data")
                return(inverse)
        }
        # If not, get the matrix, calculate the inverse and store it in
        # the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        
        # returns the inverse
        inverse
}
