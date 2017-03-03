## Use cache to save time in loops, specially if results of a calculation
## don't change between iterations.

## this function, called makeCacheMatrix, creates a list, that "saves"
## the solve of the matrix, after
## the first call of cacheSolve.
## If cacheSolve is called again, before another run of makeCacheMatrix,
## the result will be grabbed from the list.
## If makeCacheMatrix is called again, the list is flushed.

## usage: mat <- makeCacheMatrix(matrix(runif(100, 5.0, 7.5), nrow=10, ncol=10))
## usage: mat <- makeCacheMatrix(matrix(rnorm(100), nrow=10, ncol=10))
## usage: mat <- makeCacheMatrix(matrix(rnorm(s*s), nrow=s, ncol=s))
## where s <- 10 (make a matrix 10 by 10, with 100 random numbers)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  setmatrix <- function(y) { #Populate set with the matrix
    x <<- y 
    m <<- NULL
  }
  getmatrix <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(setsolve = setsolve, getsolve = getsolve,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function, called cacheSolve, will try to get cached result from
## makeCacheMatrix, if it exists.
## usage: cacheSolve(mat), where "mat" is the name of the matrix made with
## the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##start.time = Sys.time()
  
  m <- x$getsolve()
  if(!is.null(m)) {
    ##dur = Sys.time() - start.time
    ##nocache <- dur
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setsolve(m)
  ##dur = Sys.time() - start.time
  ##cache <- dur
  return(m)
  
  
}

## This function, called test, calls both makeCacheMatrix and cacheSolve
## several times, and print the results to the console window.
## usage: test(x, s) where x is the number of iterations, and s is the size
## of the matrix. (nrow = s and ncol = s)
## It is NOT advisable to input an s bigger than 1000 !!

test = function(x, s){  
  for (i in 1:x) {
    
    cat("Iteration: ", i, sep = "")
    message("")
    
    temp = makeCacheMatrix(matrix(rnorm(s*s), nrow=s, ncol=s))
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    message("This is the calculation without caching")
    print(dur)
    message("")
    
    start.time = Sys.time()
    cacheSolve(temp)
    dur = Sys.time() - start.time
    message("This is the calculation with caching")
    print(dur)
    message("")
    
  }
}