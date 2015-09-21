## The makeCacheMatrix function creates a list that caches values which are then processed by the cacheSolve function to either get the cached inverse of a matrix or solve the inverse of the matrix and cache the solution

## The makeCacheMatrix function creates a list containing functions that do the following: set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix, get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function finds the inverse of the matrix that is stored in the makeCacheMatrix list. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache with the getinv function and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

### generate matrix and test functionality of makeCacheMatrix and cacheSolve
# 
# x <- matrix(c(4, 2, 7, 6), 2, 2)
# x
# solve(x)
# 
# z <- makeCacheMatrix(x)
# cacheSolve(z)
# cacheSolve(z)
