## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function below creates a new matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
}
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}   
    
    

## Write a short comment describing this function

## The cacheSolve function will calculate the inverse of any matrix returned by
## the function makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

        ## Return a matrix that is the inverse of 'x'
test <- matrix(c(1:10),2,5)
matrix(test)

testinv <- makeCacheMatrix(test)

cacheSolve(makeCacheMatrix)

