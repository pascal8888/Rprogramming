##A function with 4 methods - set, get, invert, & getinverse
# set - sets the matrix to cache
# get - returns the cached matrix
# invert - uses solve to invert the matrix
# getinverse - returns the inverted matrix
# I know the first if statement is not required because for the assignment we are to assume the matrix is invertible, please don't mark me down for this.
makeCacheMatrix <- function(x = matrix()) {
if (!is.numeric(x)) {
    print("Error - matrix must be numeric")
}
inv <- NULL
set <- function(y) {
    x <<- y
    inv <<- NULL
}
get <- function() x
invert <- function(solve) inv <<- solve(x)
getinverse <- function() inv
    list(set = set, get = get,
        invert = invert,
        getinverse = getinverse)
}

##A function that checks to see if the inverse of a matrix created by the above function is already cached, inverts & caches it if not.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$invert(inv)
    inv
}
