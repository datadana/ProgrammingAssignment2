## This function creates a special matrix object that can cache its inverse.
## After execution, it returns the matrix along with a list of functions that we will use 
## for the solveCache function.
##This code was heavily inspired by the sample code provided in class as well as code posted on http://www.zhihaoding.com/r/cache,
##which was retireved on Sept. 21, 2015
makeCacheMatrix <- function(x = matrix()) {
  ##sets the variables for null and the matrix
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  ##Gets the matrix.
  get <- function() x
  #gets the inverse of  the matrix
  setMatrix <- function(m) n <<-m
  #gets the matrix
  getMatrix <- function() m
  #here's the list of functions we can use in other functions
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

##This function checks to see if we have already set the inverse of the 
##matrix in the cache, and makes sure the data is the same. If we have
##cached the current data, it is returned. If not, it is calculated.
cacheSolve <- function(x, ...) {
  ##checks for the inverse of the matrix
  m <- getMatrix(x) 
  ##if there is an inverse of the matrix, retrieves it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##otherwise, this sets it
  results<-get(x)
  m<-solve(results)
  setMatrix(m)
  return(m)
}