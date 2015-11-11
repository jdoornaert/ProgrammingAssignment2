## makeCacheMatrix: returns a makeCacheMatrix object created around matrix 'x'

makeCacheMatrix <- function(x = matrix())
{
    # Initialize inverse matrix (NULL = non-existent)
    i <- NULL

    # set method (update stored matrix)
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # "get" method: retrieve stored matrix
    get <- function() x

    # "setinv" method: set inverse matrix
    setinv <- function(inv) i <<- inv

    # "getinv" method: get inverse matrix
    getinv <- function() i

    # return method list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: returns the inverse of the matrix 'x', a makeCacheMatrix object

cacheSolve <- function(x, ...)
{
    # Get cached inverse
    i <- x$getinv()

    # If cached inverse is NULL, then it is not created yet...
    if (is.null(i))
    {
        # 1. Get matrix
        data <- x$get()

        # 2. Invert matrix
        i <- solve(data, ...)

        # 3. Store inverse in cache for future use
        x$setinv(i)
    }
    else
    {
        message("Returning cached inverse...")
    }

    i
}
