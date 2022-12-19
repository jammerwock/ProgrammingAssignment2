# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse

  inv <- NULL # inverse of the matrix

  set <- function(y) { # set the value of the matrix
    x <<- y # assign the value of the matrix
    inv <<- NULL # reset the inverse
  }

  get <- function() x # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # set the inverse of the matrix
  getinverse <- function() inv # get the inverse of the matrix

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # return a list of the functions

}

# Write a short comment describing this function

cacheSolve <- function(x, ...) { # solve the matrix
  inv <- x$getinverse() # get the inverse from the cache
  if (!is.null(inv)) { # if the inverse is not null, then return it
    message("getting cached data") # print a message
    return(inv) # return the inverse
  }
  data <- x$get() # get the data from the matrix
  inv <- solve(data, ...) # solve the matrix
  x$setinverse(inv) # set the inverse in the cache via the setinverse function
  inv # return the inverse
}
