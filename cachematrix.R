##  this file contains the required function to store the 



## this is a kind of objective function, which will help caching the 
# inverse matrix of the invertible matrix. if a passed matrix is not a
# invertible matrix it will result in an error.

makeCacheMatrix <- function(x = matrix()) {
  
  # to store the inverse matrix in current funtion envi
  inverseMatrix  <- NULL
  
  # to set the invertble matrix x to y and inversematrix value to null
  setMatrix<- function(y){
    # to store the passed invest matrix into parent environment variable x
    x<<-y
    # set inversematrix value to null
    inverseMatrix<<-NULL
    
  }
  
  # return the stored base matrix
  getMatrix<- function() x
  # set the calculated inverse  matrix using this function
  setInverse<- function(inverse)  inverseMatrix<<- inverse
  # return stored inverse  matrix 
  getInverse<-function() inverseMatrix
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
  
}


## this will calucurate the inverse of the matrix using makeCacheMatrix function object
# and cache the result.  x is the makeCacheMatrix function object

cacheSolve <- function(x, ...) {
  ## get the base matrix and inverse value
  baseMatrix= x$getMatrix();
  
  inverse= x$getInverse()
  
  # if the inverse values is not null, already calculated return the same, else calclate and return.
  if(!is.null(inverse)){
    print("getting from the cache");
    return(inverse)
    
  } else {
    print("  getting the   computed value");
    x$setInverse(solve(baseMatrix));
    return(x$getInverse())
  }
  
}

permMatrix<-makeCacheMatrix(rbind(c(1,0,2),c(0,1,2),c(0,0,2)))

perm1<-permMatrix$setMatrix(cbind(c(1,0,2),c(0,1,2),c(0,0,5)))

permMatrix$getMatrix()


cacheSolve(permMatrix)

