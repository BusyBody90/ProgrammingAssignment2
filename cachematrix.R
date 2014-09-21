##The two functions makeCacheMatrix and cacheSolve can be used to calculate the inverse of a Matrix
##and stores both the original matrix and the calculated inverse matrix in external variabels that 
##can be called again later after the functions has finished execution. x stores the original matrix 
##and m stores the inverse matrix of x.
##Example how to use:
##a<-makeCacheMatrix()
##a$set(matrix(2:5,2,2))
##cacheSolve(a)


##The function makeCacheMatrix creates 2 variables in an external environment with the purpose to contain a matrix and the inverse of the matrix.
##The function returns a list of functions that can be used to access and manipulate the variables in the external environment.
##Any new object created by makeCacheMatrix will always be initialized with the value NULL for the inverse matrix
##Before the set function is used: The matrix in any new object created by makeCacheMatrix will be a [1,1] matrix with the value NA.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
  
  
}


##The first time the cacheSolve function is used on an object created by the 
##makeCacheMatrix function m is always NULL and the IF condition will be FALSE. 
##Therefore the statements in the IF condition will never be executed in the first run.
##Next it assigns the matrix created by makeCacheMatrix into data and solves the inverse 
##of the matrix and assigns it to m.
##
##Any second time the cacheSolve function is used on the same object the IF condition will be true
##and you will see the message "getting cached data". The function returns with the value for the 
##inverse matrix and stops without calculating the inverse matrix again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setsolve(m)
  m

}
