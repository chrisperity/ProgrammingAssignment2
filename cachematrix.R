## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
##
## Note: For this assignment, assume that the matrix supplied is always invertible.
 

## Create a function that keeps a matrix and the cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
        ## If set is called, set x to the passed matrix and make m NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get returns the current set value
        get <- function() x
        ## setinverse sets m to the entered value
        setinverse <- function(encl) m <<- encl
        ## getinverse gets the currently cached value
        getinverse <- function() m
  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a CacheMatrix and returns the inverse, reading from cache if available
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
