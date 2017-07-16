##computes inverse from cached data
##skips computaion and get cached inverse if the inverse has already been computed. 


##This function creates a special matrix that can cache its inverse.

makeCacheMatrix<- function(x = numeric()) {
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



##This function computes the inverse of the special matrix returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cacheSolve gets the inverse from the cache.

cacheSolve<- function(x, ...) {
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