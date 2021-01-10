## Put comments here that give an overall description of what your
## functions do : 
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
##
## Write a short comment describing this function
## makeCacheMatrix is a function that takes a invertible squared matrix as an argument in X
## It is composed by 4 subfunctions : set, get, setInverse, getInverse
##
## set function sets the value of the Matrix X. When set is called, it takes the local variable y and assign it 
## to the external variable X in the makeCacheMatrix environment. And it sets the external Matrix.inverse to NULL 
## (the variable Matrix.inverse act as a cache for the calculated data).
##
## get function gets the value of the Matrix from the external variable X
##
## setInverse sets the value of the local calculated data (the inversion of the Matrix) to the external variable 
##
## getInverse gets the value of the calculated data from the cache in the external variable Matrix.inverse
##
## The four functions are named in a list in order to be called with recursive symbol $ by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
            Matrix.inverse <- NULL
            set <- function(y) {
                        x <<- y
                        Matrix.inverse <<- NULL
                        }
            get <- function() {
                        x
                        }
            setInverse <- function(inverser) {
                        Matrix.inverse <<- inverser
                        }
            getInverse <- function() {
                        Matrix.inverse
                        }
            list(set = set, 
                 get = get, 
                 setInverse = setInverse, 
                 getInverse = getInverse)
}


## Write a short comment describing this function
##
## cacheSolve function will test if the data were already calculated and stored in the external variable Matrix.inverse 
## and then use and print the stored result in the variable Matrix.inverse (which act as a cache). 
## If no results are found, the function will calculate the inverse, store it into the external variable Matrix.inverse (which 
## act as a cache) and print it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            Matrix.inverse <- x$getInverse()
            if(!is.null(Matrix.inverse)) {
                        message("using data in cache")
                        return(Matrix.inverse)
            }
            Matrix.original <- x$get()
            Matrix.inverse <- solve(Matrix.original, ...)
            x$setInverse(Matrix.inverse)
            Matrix.inverse
}
