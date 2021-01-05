## This program will enable to write an R function wherein it will able to cache computations in a short period of time. 
## Using the R Language, operators such as <<- was used to appoint a value to an object or null object function.
## The function to create a cache matrix was used and then by solving the function in R, inverse of a square matrix will be obtained.

createCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("processing cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
