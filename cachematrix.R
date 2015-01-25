## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(matx, ...) {
        ## Return a matrix that is the inverse of 'x
  inv <- matx$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- matx$get()
  inv <- solve(data, ...)
  matx$setinv(inv)
  inv
}

