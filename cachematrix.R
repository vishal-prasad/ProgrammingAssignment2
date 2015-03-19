## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function is creating a matrix object by utilising set and
## get. When the cacheSolve function is executed with a matrix object passed as an
## argument then it computes the argument to see if it has been cached already, if 
## not, then it will cache that data and print the result. If the matrix object has
## been cached then it will fetch the cached data and you will see the corresponding
## message to indicate the result is being fetched as cached data. 

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object when a matrix argument is passed into it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix 
  getmatrix <- function() m 
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix argument that is passed into it
## and caches that data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("Fetching cached data...")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
