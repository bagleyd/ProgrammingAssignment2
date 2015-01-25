## makeCacheMatrix - allows a matrix value to be retrieved from cache
## cacheSolve - returns the inverse of a matrix; return value could be
##    returned from cache

## This function makes a matrix object that can be cached 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                ## declare a local variable 's'
        ## set allows us to pass in a matrix value
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ## get allows us to retrieve the matrix
        get <- function() x
        ## set inverse assigns the inverse of the matrix
        setinverse <- function(solve) s <<- solve
        ## get inverse returns the inverse of the matrix
        getinverse <- function() s
        # return a list containing the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix that is passed in
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        
        ## determine whether we should return cached data
        if (!is.null(s)) {
                message("Getting cached data.")
                return(s)
        }
        ## not using cache; get matrix
        data <- x$get()
        ## set 's' to the inverse
        s <- solve(data, ...)
        ## save the inverse to cache
        x$setinverse(s)
        ## return the inverse of the matrix
        s
}
