## 1- set the value of the matrix passed in the argument of the function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
## 2- get the value of the matrix passed in the argument of the function
   get <- function() x
## 3- set the value of the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
## 4- get the value of the inverse of the matrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## A short comment describing this CacheSolve function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
##check first if the inverse has already been calculated. If that is the case, it will obtain 
## the inverse of the matrix from the cache and skip the computation. Otherwise, it will calculate the 
## inverse of the matrix and set the value of the inverse in the cache by means of the "setinv" 
## function. 
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
