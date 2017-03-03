## The pair of functions below caches the inverse of a matrix.

## This function stores a matrix and its corresponding inverse mattrix. 
## It provides the getter and setter methods for retrieving matrix or its inverse as well as setting those values 
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will return the inverse of a given square matrix. 
## It will return the inverse from the cache if the matrix exists; else it will calculate the value
## It will also update the cache with the matrix and its corresponding inverse for future reference.
## The function also assumes the given matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
          message("Getting cached data!")
          return(inverse)
        }
        values <- x$get()
        inverse <- solve(values)
        x$setInverse(inverse)
        inverse
}
