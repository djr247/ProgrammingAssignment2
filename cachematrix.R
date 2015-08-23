## Cache Inverse of a Matrix Assignment2:
## Assignment2 is to write a pair of functions
## that cache the inverse of a matrix.

## 1 makeCacheMatrix: This function creates a special "matrix" object 
##   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InvMat <<- inverse
  getInverse <- function() InvMat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 2 cacheSolve: This function computes the inverse of the 
##   special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should retrieve 
##   the inverse from the cache..

cacheSolve <- function(x, ...) {
  
  InvMat <- x$getInverse()
  if (!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  mat <- x$get()
  InvMat <- solve(mat, ...)
  x$setInverse(InvMat)
  InvMat
}