# The inverse of large matrices can be time-costly and henceforth there might be cases where
# caching the inverse is better than computing simply computing it. The
# two functions below are used to cache the inverse of a matrix.


# makeCacheMatrix creates a special "matrix", which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(matrixInverse) mInv <<- matrixInverse
  getMatrixInverse <- function() mInv
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates the inverse of the input-matrix and 
# sets the value of the inverse in the cache via the setMatrixInverse function.

cacheSolve <- function(x, ...) {
  mInv <- x$getMatrixInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setMatrixInverse(mInv)
  mInv
}
