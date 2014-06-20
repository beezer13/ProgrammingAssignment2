### This is function which when assigned to a variable can be used
## to set a matrix (set()), print that matrix (get()), set the value of
## the inverse matrix (setmatrix()) and get the value of the inverse 
## matrix (getmatrix()) using solve()
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
## This function checks if the inverse function has already been
## calculated, if it has it prints it, if not it calculates and
## prints it after the message "getting cached data"
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
