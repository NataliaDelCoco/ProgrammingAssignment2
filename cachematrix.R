## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix = function of setters and getters 
#   for inverting a matrix. Creates an object of 
#   makeCacheMatrix type with behaviors of set, 
#   get, setinv, getinv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(new_matrix){
        x <<- new_matrix
        m <<- NULL
    }
    get <- function() x
    setinv <- function(new_invmatrix) inv <<- new_invmatrix
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function

# cacheSolve = input is a makeCacheMatrix.object.
#   calculates  the inverse of a matrix and caches it
#   OR retrieves the already calculated inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
