## These functions calculate, cache and return the inverse of a matrix
## to reduce repeated re-calculation. Ideal for use in a loop. 

## Creates a special matrix, that happens to be a list containing functions to
##      1: Set the value of the matrix
##      2: Get the value of the matrix
##      3: Set the value of the inverse
##      4: Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special matrix created above.
##      1: If the value has already been calculated, it is returned to save computation time
##      2: Else, calculate and store inverse for faster future lookups

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
