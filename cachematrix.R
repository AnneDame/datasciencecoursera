## Put comments here that give an overall description of what your
## functions do
## These functions enable to calculate the inverse of a matrix
## But, in order to cache potentially time-consuming computation, if the inverse has already been computed, these functions are able to cache the inverse.
## So that we can use it when we will need it again instead of doing the computation again.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y){
          x <<- y
          Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        if(!is.null(Inv)){
          message("getting cached data")
          return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
}

lulu <- matrix(c(1,2,3,2,1,2,3,2,1),nrow=3,ncol=3)
lolo <- makeCacheMatrix(lulu)
resultat <- cacheSolve(lolo)
resultat
solve(resultat)
