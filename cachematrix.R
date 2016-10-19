## R Programming second programming assignment.
## The following functions cache a matrix and its inverse to save the 
## invertion time consuming operation.

## This function creates a placeholder for the matrix and its inverse.
## It returns a list of functions to store and retrieve the original matrix
## and to store and retrieve a calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function gets a object 'makeCacheMatrix' and calculates its matrix 
## inverse if it is the first time it is called for this particular object,
## otherwise it returns a previouslly calculated inverse matrix.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      ## Return a matrix that is the inverse of 'x'
      inv
}
