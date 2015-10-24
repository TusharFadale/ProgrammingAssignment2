## Large Matrix inversion is costly and if the inverse matrix will be used more than
## once the following functions will allow for caching the matrix inverses in memory

## This function creates a special "matrix" obect that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function returns the inverse of matrix x. If the inverse of the matrix exists
## in memeory it will return the cached value of the inverse otherwise it will solve
## for the inverse and retrun the inverse and also save it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     s <- x$getsolve()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     s
}
