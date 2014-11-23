## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Caching the Inverse of a Matrix. 
## key benefit: Matrix inversion is usually a costly computation.
##              This function reduces the computing repeatedly. 

## Key Assumption: Matrix supplied is always invertable

## Capabilities
## 1. MakeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 2. CacheSolve: computes the inverse of the special "matrix" 
##      returned by the makeCacheMatrix function. If the inverse has already been 
##     calculated (and the matrix has not changed), then cacheSolve should 
##     retrieve the inverse from the cache.


## MakeCacheMatrix: This function creates a special "matrix" object that can 
##      cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}



