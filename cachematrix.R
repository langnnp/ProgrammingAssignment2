## makeCacheMatrix takes argument in type of matrix, initialize a cache 
## to store a invert matrix object and defines all getters/setter for
## those variables

makeCacheMatrix <- function(x = matrix()) {
  invertMartrix <- NULL
  set <- function(mt) {
    x <<- mt
    invertMartrix <<- NULL
  }
  
  get <- function() x
  setSolving <- function(imatrix) invertMartrix <<- imatrix
  getSolving <- function() invertMartrix
  list(set = set, get = get,  setSolving = setSolving,
       getSolving = getSolving)
}


## Calculate the inverse of matrix given as argument
## if the same matrix is given, return the result in cache instead

cacheSolve <- function(x, ...) {
  solvedm <- x$getSolving()
  if(!is.null(solvedm)) {
    message("getting cached inverse matrix")
    return(solvedm)
  }
  orMatrix <- x$get()
  result <- solve(orMatrix)
  x$setSolving(result)
  ## Return a matrix that is the inverse of 'x'
  result
}
