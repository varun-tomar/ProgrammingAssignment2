## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Imatrix <- NULL
  set <- function(y) {
    x <<- y
    Imatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Imatrix <<- inverse
  getinverse <- function() Imatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Imatrix <- x$getinverse()
  if(!is.null(Imatrix)) {
    message("getting cached data")
    return(Imatrix)
  }
  data <- x$get()
  Imatrix <- solve(data, ...)
  x$setinverse(Imatrix)
  Imatrix
}
