## This pair of functions caches the inverse of a matrix rather than compute it
## repeatedly, to save computation resource when the inverse is frequently used.

## makeCacheMatrix generates a special object containing the matrix itself and
## its inverse, as well as methods to set/get the matrix itself and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## cacheSolve return the inverse of the special matrix created by function above.
## It first checks whether the inverse has already been calculated. If yes, it
## returns it directly. Otherwise it calculates the inverse and cache into the 
## special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inv(inv)
  inv
}
