## This function returns a list containing 4 functions:
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns a matrix that is the inverse of'x'
## It either gets the inverse if it has been previously 
## calculated, or will calculate the inverse of 'x'.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
