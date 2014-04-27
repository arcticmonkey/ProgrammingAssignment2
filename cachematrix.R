## The first function takes a matrix as input, and specifies which function
## is to be applied to the matrix. The second function checks the cache
## if it has any information on the inverse of matrix, if yes it takes the
## value from the matrix, otherwise it calculates the value of inverse of
## matrix and return.

## makeCacheMatrix has information about which function is to be
## applied to the martix. In this case we are using solve() to get
## the inverse of the martix. This functions doesn't actually do the
## inverse calculation, it just specefies the function to be applied.

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


## I pretty much said everything about this function at the top of this
## page. Try to run these functions, they run quite well.

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
