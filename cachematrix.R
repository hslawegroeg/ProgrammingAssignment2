## Computing the inverse of a matrix is costly, this code lets us cache
##the inverse the first time so we can use the cached value in subsequent
##code as long as the data in the matrix hasn't changed

## This creates a matrix which can cache its inverse using solve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This code computes the inverse of the matrix unless there is already
##a value for the inverse in cache, in which case that will be returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setsolve(m)
  m
}

##Thank you for reviewing!
