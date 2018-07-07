## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set up the flag for a stored inverse
  inv <- NULL
  
  ## Set up the matrix and clear previous data
  setMtx <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  getMtx <- function() {
    x
  }
  
  ## Set the inverse
  setInv <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse
  getInv <- function() {
    inv
  }

  ## The results
  list(setMtx = setMtx, 
       getMtx = getMtx, 
       setInv = setInv, 
       getInv = getInv)
  
}

## Checks for stored solutions, stores a solution,
## and then solves.

cacheSolve <- function(x, ...) {
  
  ## Fill in inv
  inv <- x$getInv()
  
  ## Check if inv is storing anything and return it
  if(!is.null(inv)) {
    message("Matrix already stored. Retrieving...")
    return(inv)
  }
  
  ## Retrieve the matrix and solve it
  mat <- x$getMtx()
  inv <- solve(mat, ...)
  
  ## Store the inverse and return it
  x$setInv(inv)
  inv
}






