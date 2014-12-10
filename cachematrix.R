## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix  <- NULL
  
  setMatrix<- function(y){
    
    x<<-y
    
    inverseMatrix<<-NULL
    
  }
  
  getMatrix<- function() x
  
  setInverse<- function(inverse)  inverseMatrix<<- inverse
  
  getInverse<-function() inverseMatrix
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
   
    
    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
           baseMatrix= x$getMatrix();
           inverse= x$getInverse()
           
           if(!is.null(inverse)){
             
             print("getting from the cache");
             
             return(inverse)
             
           } else {
             
             print("  getting the   computed value");
             x$setInverse(solve(baseMatrix));
             
           }
           
           return(x$getInverse())
  
  
  
  
}


permMatrix<-makeCacheMatrix(rbind(c(1,0,2),c(0,1,2),c(0,0,2)))
perm1<-permMatrix$setMatrix(cbind(c(1,0,2),c(0,1,2),c(0,0,5)))

permMatrix$getMatrix()


     cacheSolve(permMatrix)

