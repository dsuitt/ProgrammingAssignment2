# makecachematrix is a function that will cache the inverse of a matrix
# makecachematrix then creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#function assumes input is a square matrix

makeCacheMatrix <- function(x = matrix()) {
  
  mymatrix <- NULL
  
  set<- function(y) {
    x<<- y
    mymatrix <<- NULL
  }
  
  get<- function() x
  setinverse<- function(inv) mymatrix<<- inv
  getinverse<- function() mymatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


# cachesolve returns the inverse of an input matrix by checking if the 
# matrix has already been solved and returning that value. Otherwise
# it computes the inverse, caches it, and returns the value to the parent
# environment

cacheSolve <- function(x, ...) {
  
  mymatrix <- x$getinverse()
  
  if(!is.null(mymatrix)) {
    message("getting cached data")
    return(mymatrix)
  }
  
  data <- x$get()
  mymatrix <- solve(data)
  x$setinverse(mymatrix)
  mymatrix
  
  
}
