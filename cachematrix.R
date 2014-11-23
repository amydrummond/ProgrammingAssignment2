## These functions solve the inverse of a matrix and 
## caches the solutions for future use.


## This function returns a list of functions which
## cache the solution of a matrix

makeCacheMatrix <- function(x = matrix()) {
  solution <- NULL
  set <- function(y) {
    x <<- y
    solution <<- NULL
  }
  get <- function() x
  setSolved <- function(matrixInversed) solution <<- matrixInversed
  getSolved <- function() solution
  
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)  
}



## This function either gets a cached matrix or sets the inverse of a matrix to the cache. 

cacheSolve <- function(x, ...) {
  solution = x$getSolved()
  if (!is.null(solution)) {
    message("getting cached data")
    return(solution)
  }
  data <- x$get()
  solution <- solve(data, ...)
  x$setSolved(solution)
  solution
}
