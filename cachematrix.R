### makeCacheMatrix
# makeCacheMatrix returns a list that contains four functions:
# set: saves the input y (the matrix in this application) in the parent environment
#      and also sets the inverse to NULL which overwrites any previous inverses.
#      This leads to a recalculation of the inverse when a new matrix is set
#      because cacheSolve checks if inv is NULL.
# get: returns the the matrix
# setinverse: saves the result of the calculation (which should typically be the input)
#             in the parent environment as the object inv.
# getinverse: returns inv (can be NULL)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getinverse <- function() inv
  # return:
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# cacheSolve first checks if the inverse can be found in the cache.
# If it can be found the function just returns that value and the message "getting
# cached data". Otherwise the inverse is calculated, saved using setinverse and
# returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse() # function() inv
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # return ends the function
  }
  data <- x$get() # function() x
  inv <- solve(data, ...) # calculates the inverse and stores it as inv
  x$setinverse(inv) # function(inverse) inv <<- inverse
  inv
}
