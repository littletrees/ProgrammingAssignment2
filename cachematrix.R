## makeCacheMatrix function creates a special matrix, which can store its inverse. cacheSolve returns the inverse, if the inverse is cached it returns the inverse without computing again. 

## This function creates a special "matrix", which is really a list containing a function to set the value of the vector, to get the value of the vector, to set the value of the mean, and to get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the special matrix created from the above function. If the inverse is cached already, it'll be returned directly. Or the funciton calculates the inverse and then returnes the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x)
  inverse
}
