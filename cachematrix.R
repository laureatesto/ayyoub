
## This function is used for setting and getting the values required.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL()
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function is used for getting the inverse of the matrix as a result.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
            message("Retrieving the required cached data.")
            return(m)
        }
        data <- x$get()
        x <- solve(x, ...)
        m$set_inverse(m)
        m
}
