## Below functions take a squared matrix as argument and calculates 
## the inverse of it for the first time and stores the result in cache for
## subsequent retrievals


## makeCacheMatrix function takes a matrix as argument and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setinvm <- function(Solve) invm <<- Solve
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
  }



## cacheSolve function takes a matrix as a first argument and returns its inverse matrix
## in this function the inverse matrix is calculated once for the first time and stored in Cache 

cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached inversed matrix data")
    return(invm)
  }
  matx <- x$get()
  invm <- solve(matx)
  x$setinvm(invm)
  invm
}
