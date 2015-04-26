# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve returns the inverse of the matrix.
# If the inverse has already been computed, it returns cached inverse.
# If not, it computes the inverse, sets the value in the cache, and then returns it.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not yet calculated, calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}
