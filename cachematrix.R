
## Makes the Cache Matrix and returns a list with functions 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    if(!identical(x,y)) x<<-y  #Avoiding Unnecessary Assignment
    i<<-NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function(inverse) i
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Gets cached data, if data not found computes inverse and return inverse matrix

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i<-solve(mat, ...)
  x$setinverse(i)
  i
  
  ## Returning a matrix that is the inverse of 'x'
}
