## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMtx <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getMtx <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  list(setMtx = setMtx, 
       getMtx = getMtx, 
       setInv = setInv, 
       getInv = getInv)
  
}

## Checks for stored solutions, stores a solution,
## and then solves.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Matrix already stored. Retrieving...")
    return(inv)
  }
  
  mat <- x$getMtx()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}





