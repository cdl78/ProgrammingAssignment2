
## Example Usage
##
## create a makeCacheMatrix object
## > x <- $makeCacheMatrix
## set matrix value using set() function (only allows square matrices)
## > x$set(M1)
## solve for inverse, passing in x
## > cacheSolve(x)

## try solving it again and you will see that the inverse is
## read out from the cache
## > cacheSolve(x)

# > B
# [,1] [,2] [,3]
# [1,]    1   44    7
# [2,]    2    5    8
# [3,]    3    6    9
# > x <- makeCacheMatrix()
# > x$set(B)
# > cacheSolve(x)
# [,1]   [,2]       [,3]
# [1,] -0.0125 -1.475  1.3208333
# [2,]  0.0250 -0.050  0.0250000
# [3,] -0.0125  0.525 -0.3458333
# > cacheSolve(x)
# getting cached data
# [,1]   [,2]       [,3]
# [1,] -0.0125 -1.475  1.3208333
# [2,]  0.0250 -0.050  0.0250000
# [3,] -0.0125  0.525 -0.3458333
# > 

### This function creates a matrix object with get & set for the matrix
### and get & set for the matrix inverse
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

### this function returns the inverse of matrix passed into
### makeCacheMatrix
### if it has been previously calculated it will return the cached value
### if not, it will freshly calculate the value

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

