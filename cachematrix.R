#
# Caching the Inverse of a Matrix
#
# Matrix inversion is usually a costly computation and their may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly.
#
# Functions:
#
# 1.- makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2.- cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
#     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will
#     retrieve the inverse from the cache.
#

# Create the wrapper makeCacheMatrix function which encapsulate the list of methods defined
# to be used in the cacheSolve function (set, get, setinverse and getinverse)
makeCacheMatrix <- function(x = matrix()) {
  # Checking some errors
  if(!is.matrix(x)){
    stop("matrix expected")
  }
  if(nrow(x) != ncol(x)){ 
    stop("only square matrices can be inverted")
  }
  
  inverseMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  # Return the list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Solve the inverse of a square matrix
cacheSolve <- function(x, ...) {
  # Checking if the inverse matrix is in cache
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    # return the cached matrix
    return(inverseMatrix)
  }
  
  # Calculate the inverse matrix (usually a long time operation ...)
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  
  # Return the calculated inverse
  inverseMatrix
}


# Practical example
mdat <- matrix(c(1, 2, 11, 12), nrow = 2, ncol = 2)
matrix1 <- makeCacheMatrix(mdat)
cacheSolve(matrix1)
cacheSolve(matrix1)

# Checking errors
makeCacheMatrix(c(1))                                   # Error: matrix expected
makeCacheMatrix(matrix(c(1,2,3,4,5,6), nrow=2, ncol=3)) # Error: only square matrices
