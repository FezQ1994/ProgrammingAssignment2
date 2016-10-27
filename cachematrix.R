## Together, the two functions take a matrix and solve it. If the matrix has already been solved,
## then makeCacheMatrix returns the inverse immediately after printing a message. Else,
## cacheSolve will return the inverse using the solve() function

## Takes a matrix, x, and creates a list containing previously inputted matrices and their 
## respective inverses

makeCacheMatrix <- function (x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## obtains the inverse, if it exists, from the makeCacheMatrix function. If it doesn't,
## it will calculate the inverse.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  x$setinv(inv)
  return(inv)
}
