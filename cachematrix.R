## This is a pair of functions that cache the inverse of a matrix.
## The first function makeCacheMatrix caches the inverse of the matrix if it exists. 
## The second one calculates the inverse of the matrix or retrieves the inverse matrix
## if the calculation has already been made once.

## The function makeCacheMatrix creates a "special" matrix which is actually a list
## containing 4 functions that:
## 1.set the value of the matrix to be inverted
## 2.get the value of the matrix to be inverted
## 3.set the inverse matrix
## 4.get the inverse matrix

##

makeCacheMatrix <- function(x = matrix()) {
  inv<<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inv <<- inver
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes as an argument the "special matrix" that is returned
## from makeCacheMatrix.
## It returns the inverse of the matrix that has been set in makeCacheMatrix.
## The function calculates the inverse matrix using "solve", if it has not already 
## been calculated before.In this case it retrieves its cached value.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
