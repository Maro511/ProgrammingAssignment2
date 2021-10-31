## Functions to create and cache the inverse of a square matrix
## Final submission

makeCacheMatrix <- function(x = matrix()) {
  ##Function that create the inverse of a squared matrix and cache it
  ##Returns of list of functions to set and get the squared 
  ##matrix, to set and get the inverse matrix
  
  InvM <- NULL
  #set the matrix
  set <- function (y) {
    x <<- y
    InvM <<- NULL
  }
  #get the matrix
  get <- function () x
  #set computation of the inverse matrix and cache it
  setInv <- function(solve) InvM <<- solve
  #function to get the inverse matrix
  getInv <- function () InvM
  list (set = set, get = get, 
        setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  ##Function that computes the inverse of a squared matrix, first
  ##checking to see if such matrix already exists
  ##x = output of makeCacheMatrix function
  
  InvM <- x$getInv()
  #Look for an existing cached inverse matrix
  if(!is.null(InvM)){
    message("getting cached inverse matrix")
    return(InvM)
  }
  #Get the square matrix
  SquareM <- x$get()
  #Computes the inverse of the square matrix
  InvM<- solve(SquareM, ...)
  x$setInv(InvM)
  #return the inverse of the input matrix
  InvM
}

## Example to test functions
set.seed(102540)
SquareM<-matrix((runif(2500, max = 100)), nrow=50, ncol = 50)
TempSquareM<-makeCacheMatrix(SquareM)
cacheSolve(TempSquareM)
