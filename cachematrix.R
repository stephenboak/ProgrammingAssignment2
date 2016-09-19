## Function makeCacheMatrix creates an object that stores a vector and its inverse.
## This function then forms the argument passed to the function cacheSolve that
## retrieves the cached value of the inverse matrix that is stored in the first
## functions environment where it was created. Where no inverse matrix has been 
## cached the second function calculates it and stores it in the original environment.

## This function reads in and initialises the input value as a matrix and 'clears
## out' inv, the variable created to hold the inverse matrix. It also creates
## 'behaviours' to "get" and "set" the various values of the input matrix and its
## inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function takes the argument 'makeCacheMatrix(x)' and looks for the
## inverse matrix in the original environment with a 'get' behaviour (see above).
## It then tests to see if the inverse has in fact been cached in the original
## environment and if it has returns a message stating that it is "getting cached
## data" and once retrieved it returns the inverse matrix. If the inverse matrix
## has not been cached it calculates the matrix, 'sets' it in the original
## environment and returns its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
