## makeCacheMatrix is a consist of: set the matrix, get the matrix, set the inverse of matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function calculated the inverse check to see if the inverse has been calculated before. 
##If it is not a null, then the inverse can be returned from cache data. Otherwise, it'll calculate the inverse. 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

##Testing
mat <- makeCacheMatrix(matrix(2:7, 2, 2))
mat$get()
mat$getinv()
cacheSolve(mat)
cacheSolve(mat)
mat$getinv()
