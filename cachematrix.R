## The functions calucate the inverse of a given matrix and cache the result


## This function get a new matrix and cache it's inverse.
## This function avoids computering repeated and saves time.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() {x}
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function checks whether the inverse has been calucated.
## If so, the function returns the previous result; if not, it calucate the inverse 
## and save it to function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
