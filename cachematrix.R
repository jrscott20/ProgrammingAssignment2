## These functions create a special object that stores a matrix and 
## cache's its inverse.

## The first function, makeCacheMatrix, creates a list containing functions
## to 
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The second function calculates the inverse of the matrix that's
## held in the structure created by the above function if it has not
## already been calculated. If it has already been calculated, then 
## instead, it pulls the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
