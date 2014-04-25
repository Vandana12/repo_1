## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv) inv <<- inv
  # to get the inverse
  getinv <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    

}


## Write a short comment describing this function
## The following function calculates the inverse of the special 
## "matrix" created with the above function. However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse is already cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # not cached, so we get the matrix into data
  data <- x$get()
  # and compute the inverse
  inv <- solve(data)
  # then cache the inverse
  x$setinv(inv)
  # and return it as well
  inv
}
