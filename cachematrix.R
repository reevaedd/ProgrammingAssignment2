## The following pair of functions compute the inverse of a matrix 
## and cache the result to avoid recomputation that is usually costly

## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The function cacheSolve computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	 m <- x$getsolve()
        if(!identical(m, matrix())) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
