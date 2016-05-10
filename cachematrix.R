## Calculating the inverse of a square Matrix:
## Caching the calculation is so important to save time and resources
## So we are creating a function called "cacheInverse" to cache the result rather than compute it every time.
## First we create a function called "makeCacheMatrix"that creates , sets the values and get the inverse.
## and a second one that stores created matrix and caches its inverse.

## creating makeCachematrix first

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { 
    # here x is a gloval value and can be called outside this scope
    x <<- y
    # same for inv variable
    inv <<- NULL  
  }  
  get <- function() {return(x)}
  
  setInverse <- function(solve){ inv <<- solve}
  
  getInverse <- function() {inv}
  
  # just a test inside my functio to print out the resluts during the function call
  if(is.null(inv))
    cat("Sorry !! The inverse of this matrix is currently Null , you need to calculate it")
  else
    cat("the inverse of the \"special\" Matrix is :", inv)
  
  list(setMatrix = set, getMatrix = get, setInverse = setInverse, getInverse = getInverse)
  
}


## creating a second function cacheInverse that caches the inverse.

cacheInverse <- function(x, ...) {
  # calling global variable outside the scope 
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  matrice <- x$get()
  inv <- solve(matrice, ...)
  x$setInverse(inv)
  inv
}

