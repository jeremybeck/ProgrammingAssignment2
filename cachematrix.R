## Functions for Creating a Custom Matrix object with associated "Inverse" property (makeCacheMatrix)
## And a second function that solves for the inverse of a specified matrix, checks if the inverse property
## of the matrix has already been calculated, and retrieves or calculates that inverse. (cacheSolve)


## Create a Custom Matrix Object to Store specified matrix
## in addition to its inverse, and a series of helper functions
## to store or retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) {inverse <<- inv}
        getinverse <- function() {inverse}
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}        


## Function to Generate the Inverse of a Specified Matrix object:
## Function first checks if the inverse exists in the matrix object
## and if so, returns the stored inverse.  If the inverse does not exist
## the function calculates and returns the inverse, and appends the matrix inverse
## in the matrix object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}


