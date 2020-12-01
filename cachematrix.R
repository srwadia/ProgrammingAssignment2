## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Notes: Similar to the makeVector function in the example code, this function 
# does the following 4 things: 
# 1). it initializes objects 
# 2). it creates a set() function that assigns an input argument to an object in the parent environment
#     & clears inverse values associated with previous executions of cacheSolve
# 3). it creates a function get() that retrieves data from the parent environment 
# 4). it uses the <<- assignment operator to assign an input argument to object "m" in the parent environment
# Finally, it returns a list of functions (each element in the list is a function: set, get, setinverse and 
# getinverse)

# Credit: Len Greski who previously took the course and provides a comprehensive explanation of the example
# code on GitHub 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

# If a valid inverse exists in the cache, this function will return that value
# If the value in the cache is "NULL", the inverse is calculated using the built-in function solve
# and then returned to the parent environment using the setinverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
