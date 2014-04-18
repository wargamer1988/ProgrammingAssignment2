## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #function to set the value of the object
  setMatrix <- function(y) {  
    x <<- y
    m <<- NULL
  }
  
  #function to extract the value from the object.
  getMatrix <- function() x   
  
  #function to store the given value as mean for future use.
  setInverse <- function(inverse) m <<- inverse
  
  #function to extract the mean
  getInverse <- function() m
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  #if inversed matrix is not null, then return the cache directly
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if inversed matrix is null, then inverse the Matrix
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
