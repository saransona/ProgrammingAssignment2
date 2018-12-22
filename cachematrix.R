## Put comments here that give an overall description of what your
## functions do

## The below 2 functions makeCacheMatrix and cacheSolve help us to create a matrix, calculate
## its inverse and then retrive it from the cache assuming the inverse has not changed



## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
## The matrix created serves as a list of 4 functions,one each to 

## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. Set the value of inverse
## 4. Get the value of inverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  }


## Write a short comment describing this function

## This function is to retrive the inverse of the matrix created by the makeCachematrix
## function above from the cache. This is helpful as we come across certain situations 
## where we need to repeatedly need to calculate or retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Retrieving the inverse of matrix from Cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
