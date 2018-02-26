## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## it is really a list that contains a function to
## set the values of the matrix, get the values of the matrix
## set the values of the inverse, get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inv <<- solve
      getInverse <- function() inv
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

## This function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function above
## If the inverse has already been calculate, then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
