## Function creates a matrix that caches matrix inverse. 

## Function "makeCacheMatrix" contains four other functions - "set" for setting matrix, "get" for retrieving the matrix,
##"setinv" for calculating the inverse and "getinv" for retrieving cached inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
##This function calculates the inverse of matrix created in "makeCacheMatrix" function. BUT at first it checks whether 
##the inverse has already been calculated. If it was, the it gets inverse from the cache, otherwise it calculates the
## inverse of the data and sets the inverse value in the cache.

cacheSolve <- function(x, ...) {
m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
    
##Sample run:
## b <- makeCacheMatrix(x)
##> cacheSolve(b)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(b)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
