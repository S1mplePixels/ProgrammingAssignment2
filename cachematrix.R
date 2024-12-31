# Generic function to create a matrix object on which you can get the inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize the matrix to NULL
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }

        setinverse <- function(inv) {
                inverse <<- inv
        }
        getinverse <- function() {
                inverse
        }
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


# Get the inverse of matrix using cache if available.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()

        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
