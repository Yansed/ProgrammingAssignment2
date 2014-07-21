## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function makeMatrix returns a list of functions. set function is to set the inverse function to a given or computed matrix;
## get function is to get the matrix that need the inverse; setinverse is used to store the computed inverse matrix; and getinverse
## function is for getting or returning the stored inverse matrix.


makeMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function
## Function cacheSolve take list outcome from makeMatrix function, check if the inverse matrix is there (not null), if it is empty, this function compute
## the inverse function from the given matrix, store and return the inverse; otherwise, it returns the currently stored inverse matrix.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting inversed matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}



