## Put comments here that give an overall description of what your
## functions do
## we have two functions
## 1-makeCacheMatrix->build up a matrix and calculate the inverse, storing it in cache
## 2-cacheSolve->calculation of the inverse matrix in case there is no cache of it
## makeCacheMatrix is a function creating a special matrix, which is a list containing a function to 
## 1-set the value of the matrix
## 2 get the value of the matrix
## 3-set the value of the inverse of the matrix
## 4-get the value of the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calculates, if there is no value cached, the inverse of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
}
