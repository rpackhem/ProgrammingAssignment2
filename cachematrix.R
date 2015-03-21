## Code will return the inverse of a matrix X (X must be a square, invertable matrix)

## First get and set cache values
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
  x <<- y
  i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Then return matrix inverse
cacheSolve <- function(x, ...) {
  ## Check if matrix inverse (i) already exists
  ## If it does, then give user message indicating the cached data is being returned
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If it does not exist, then use the solve function to calculate and return value i
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
