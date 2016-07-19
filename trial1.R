makeCacheMatrix <- function(x = matrix()) {
 
  m <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setInverse <- function(inverse) m <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() m
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  m <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(m)) {
    message("getting cached data")
    
    # display matrix in console
    return(m)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
 {
    # set and return inverse of matrix
    m <- solve(matrix, ...)
  
  finally = {
    # set inverted matrix in cache
    x$setMatrix(m)
  } 
  
  # display matrix in console
  return (m)
}



a <- makeCacheMatrix()    
a$set(matrix(1:4, 2, 2))   
cacheSolve(a)        