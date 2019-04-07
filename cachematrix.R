# The first function, makeCacheMatrix, creates a special "matrix" object 
# that can cache its inverse. The second function, cacheSolve, computes 
# the inverse of the special "matrix" returned by makeCacheMatrix

# makeCacheMatrix gets a matrix as an input (rows 9-10), set the value of that matrix (rows 11-14), 
# get the value of the matrix (row 15), then set the inverse matrix (row 16) and finally get the
# inverse matrix (row 17)

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL 
  setMatrix <- function(y) {
    x <<- y 
    invMatrix <<- NULL 
  }
getMatrix <- function() x 
setinverse <- function(inverse) invMatrix <<- inverse
getinverse <- function() invMatrix
list(setMatrix = setMatrix, getMatrix = getMatrix, 
     setinverse = setinverse, 
     getinverse = getinverse)
}


# cacheSolve takes as an input the output of the first function makeCacheMatrix (a matrix) and checks
# if inverse matrix provided by makeCacheMatrix has any value in it or not (rows 30-31). If inverse matrix
# from makeCacheMatrix has some value in it,cacheSolve returns the message in row 33 and cache inverse 
# matrix (rows 32-35). If inverse matrix has not any value in it, then cacheSolve gets the original matrix 
# and set the inverse with the solve function (rows 36-40).  

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse() 
  if (!is.null(invMatrix)) {
    message("getting cached invertible matrix")
    return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
  }





