## The file contains a pair of functions that cache the inverse of a matrix.

# The first function (makeCacheMatrix) creates a matrix.
# It then defines functions, which are used to access and set 
# the values of the matrix. In the end it creates a list of those functions. 

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


# Example to show how the function works:

## First, the matrix with two rows and two columns is created using 
## makeCacheMatrix function:
matrix1 <- makeCacheMatrix(matrix(c(4,2,7,6), nrow=2,ncol=2))

# Then, by using cacheSolve function, the inverse of the matrix is created:
cacheSolve(matrix1)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

# Subsequent use of cacheSolve function reads the previously calculated inverse
# of the matrix from memory; message appears "getting cached data":
cacheSolve(matrix1)

# getting cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
