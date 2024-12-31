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
