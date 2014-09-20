## The purpose of this assignment is to be able 
## to cache costly computations. If the computation
## value alrady exists it can get it from cache

## First function makeCacheMatrix  creates a  special matrix object that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
  
  matrixi = null 
  print(x)
  set <- function(y) {
    x <<- y
    matrixi <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixi <<- solve
  getinverse <- function() matrixi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse exists in cache
## it will read the object otherwise call the makeCacheMatrix 
## function to get the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixi <- x$getinverse()
  if(!is.null(matrixi)) {
    message("getting cached inverse matrix")
    return(matrixi)
  }
  data <- x$get()
  matrixi <- solve(data, ...)
  x$setinverse(matrixi)
  matrixi
}
