## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  # Retrieve the cached inverse, if available
  
  # Check if the inverse is already calculated and cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}
