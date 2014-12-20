## Made by: Carlos Wilson, Tabasco, México.
## I write a pair of functions that cache the inverse of a matrix.
## We have the following functions:
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## ...and...
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## MAKECACHEMATRIX
## ThIS function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setinverse <- function(m.inverse) m.inv <<- m.inverse
  getinverse <- function() m.inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CACHESOLVE
## This function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

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


## Example execution:

## makeCacheMatrix(matrix(c(2, 2, 3, 2), 2, 2))
## cacheSolve(makeCacheMatrix(matrix(c(2, 2, 3, 2), 2, 2)))
