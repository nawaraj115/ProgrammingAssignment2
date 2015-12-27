## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# making cache matrix and retruning a list of functions
makeCacheMatrix <- function(x = matrix()) ##stores the cache value
{
  inv <- NULL##Initialize to null
  set <- function(y) ##creates the matrix in the working environment.
    
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x###Get the value of the matrix
  setInverse <- function(inverse) inv <<- inverse##Inverse the matrix and store in the cache
  getInverse <- function() inv##Get teh inverted matrix from the cache
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)###Return the created functions to the working environment
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) 
{
  ## attempt to get the inverse of the matrix stored in cache
  inv <- x$getInverse()#
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() # create matrix since it does not exist
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
#########################################
##Callling FUNCTION for verification.....
########################################
##my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##my_matrix$get()
##my_matrix$getInverse()
##cacheSolve(my_matrix)
##my_matrix$getInverse()
###
