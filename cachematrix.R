## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function (makeCacheMatrix) creates a matrix.
#which is really a list containing a function to

#set the value of the matrix?????
#get the value of the matrix??????
#set the the inverse of the matrix
#get the the inverse pd the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# The second function (cacheSolve) calculates the inverse of the matrix 
# created with the first function, unless it was already calculated. 
# The cacheSolve function calculates the inverse matrix using the solve function
# and sets it in the cache.
# If the inverse matrix was already calculated before,the cacheSolve function
# gets the inverse matrix from the cache and skips the calculation. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
