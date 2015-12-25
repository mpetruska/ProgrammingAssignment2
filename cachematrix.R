## Put comments here that give an overall description of what your
## functions do

## Creates a "cachematrix" object that will cache the inverse of the
## original matrix.
## Members:
## * get()              : returns the matrix
## * set(x)             : sets the matrix to x, clears the inverse cache
## * getInverse(...)    : returns the inverse from the cache (if calculated) or
##                        calculates the inverse and stores it in the cache

makeCacheMatrix <- function(x = matrix()) {
    matrix <- x
    inverse <- NULL
    
    structure(
        list(
            get = function() matrix,
            set = function(x) {
                matrix <<- x
                inverse <<- NULL
            },
            getInverse = function(...) {
                if (is.null(inverse))
                    inverse <<- solve(matrix, ...)
                inverse
            }
        ), class = "cachematrix")
}


## Returns the inverse of the matrix stored in x.
## If the inverse is already calculated the result will
## be read from cache.
## (Calls x$getInverse(...) internally.)

cacheSolve <- function(x, ...) {
    x$getInverse(...)
}
