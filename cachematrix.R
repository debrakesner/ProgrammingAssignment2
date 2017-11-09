## Paired functions below, cache the inverse of a matrix for increased speed of computation

## The following creates a list containing a fuction to set/get the value of the matrix
## and set/get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
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

## The following calculates the inverse matrix of the special 'matrix' created with the above function
## the function first checks if the inverse has been calculated prior
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse ()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}