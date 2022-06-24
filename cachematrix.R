## This functions are to cache a matrix and the inverse

## This first function creates a special object that stores a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This second function computes the inverse of the "matrix" created by makeCacheMatrix function above.
## If the inverse matrix is the same as previously calculated, then it retrieves the previous inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## inv <- x$getInverse()
  if (!is.null(inv)) {
          ## check if the return invMatrix is identical
    if ( identical( x$getInverse() %*% inv, inv %*% x$getInverse() ) ){
        ## get it from the cache and skips the computation. 
        print("getting cached data")
        return(inv)
    }
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
