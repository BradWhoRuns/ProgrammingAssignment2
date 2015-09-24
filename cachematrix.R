## This is the makeCacheMatrix function.  It stores and retrieves the inverses
# of matrices.

# I used the variable names mat and invs for the original matrix and its inverse, resp.  
# likewise, I used the function names, setmatrix, getmatrix, setinvs and getinvs.

makeCacheMatrix <-function(mat=matrix()){
  
  invs<<-NULL
  
  setmatrix<-function(y){
    
    mat<<-y
    invs<<-NULL
  }
  getmatrix<-function(){mat}
  
  setinvs<-function(solve){invs<<-solve
  }
  
  getinvs<-function(){invs}
  
  
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinvs=setinvs, getinvs=getinvs)      

}    


## This is the cachesolve function

# It takes the makeCacheMatrix function as its input
# The argument of the makeCacheMatrix function should be the matrix that you want to invert



cachesolve <- function(x, ...) {
  m <- x$getinvs()
  
  if(!is.null(m)) {
    
    message("Retrieving cached inverse...")
    
    return(m)
  }
  
  message("Computing inverse...")
  hold_matrix <- x$getmatrix()    # Here I used the variable hold_matrix
  
  m <- solve(hold_matrix)
  
  x$setinvs(m)
  
  m
  
  
}