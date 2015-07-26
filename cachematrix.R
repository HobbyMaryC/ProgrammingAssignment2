## Assignment 2:  Inverse caching: use free variable, super assignment operator
## 

## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv.in) inv <<- inv.in
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Computes the inverse of the special matrix returned by makeCacheMatrix().
# If the inverse has been calculated, and the matrix has not changed,
# returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv2 <- x$getinv()
  if (!is.null(inv2)) {
    message("getting cached data")
    print("access cache")
    inv2				# Opt to print the value also			
    return(inv2)
  }
  print("calculating inverse")
  cache.matrix <- x$get()
  inv2 <- solve(cache.matrix)
  x$setinv(inv2)
  inv2					# Opt to print the value also
  return(inv2)
}
