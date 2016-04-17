# The main aim is to save the system resources for the repeated computation
#Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. Cache stores the data internally
#and speeds up the subsequent retrivals. Very useful in scenarios where it queries large data when data between
#the last retrival and now is not refreshed.


## This function create special matrix and will cache its inverse
#the function can be described basically into 4 components
#Set the value of the matrix
#get the valude of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This is the main function that cahche functionality executes
#does the inverse of the matrix
#checks to see if the inverse is already computed
#if yes, its gets the cached value- here is where is saves the system resource
#by not computing again
#if not, it computes the inverse and stores it in cache


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
