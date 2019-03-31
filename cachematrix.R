###The function below creates a special matrix object that can cache its inverse. This special matrix is
  #really a list which contains the following functions to:
  #1. Set the value of matrix
  #2. Get the value of matrix
  #3. Set the inverse of the matrix
  #4. Get the inverse of the matrix

makeCacheMatrix <- makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

###The function below calculates the inverse of the special matrix created by the function above. If the matrix
  # has been calculated already, the it will retrieve its value from the pervious function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


