## Calculate the inverse of matrix and stores it in the cache

## function that set and get value of a matrix, 
## and set and get value of a inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {    #function that set values of matrix and the beginning inverse of matrix
    x <<- y               #create value of x (matrix) in the parent environment
    inv <<- NULL          #create value of inv in the parent environment
    
  }
  get <- function() x     #get value of matrix
  setinversion <- function(solve) inv <<- solve   #create value of inv (inverse of matrix) 
                                                  #in the parent env
  getinversion <- function() inv                  #get value of inverse of matrix 
                                                  #that is stored in parent env.
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## function that return inverse of matrix 'x'

cacheSolve <- function(x, ...) {

  inv <- x$getinversion() #create in local env. variable 'inv'
                          #that call the fun. which has inverse of matrix

  if(!is.null(inv)) {
    ##if there is an inverse of matrix, then read this value
    message("getting cached data")
    return(inv)          #return inverse of matrix
  }
  data <- x$get()         #create in local env. 'data' that read the matrix
  inv <- solve(data, ...) #create in local env. 'inv' that stores inverse of matrix
  x$setinversion(inv)     #call the funcion that will store inverse of matrix in cache
  inv
}
