## Put comments here that give an overall description of what your
## functions do

# This "makeCacheMatrix" function creates a special "Matrix", which is really a list of:
#1. set the value of the matrix 
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix
  
  I <- NULL
  # Set the inverse equal to NULL
 
  set <- function (y){
    x <<- y
    I <<- NULL
  }
  #sets x to be the matrix y and resets I to be null
  
  get <- function() x
  # get function returns the matrix
  
  setinverse <- function(inverse) I <<- inverse
  # setinverse overrides the previous value of I and assigns the argument to Inverse (which is supposed to be the inverse of matrix x)
  
  getinverse <- function() I
  #Returns the inverse
  
  list(set = set, get = get, setinverse = setinverse      , getinverse = getinverse)
  #Creates a list of the functions
}


## This function calculates the inverse of the Matrix created above and can be read by using the "x$get" unction
#Firts it checks if the inverse exists it will be returned without any calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  I <- x$getinverse()
  # Retrives the most recent value for the inverse
  
  if(!is.null(I)){
    message("retrieving calculated inverse")
    return(I)
  }
  #if the inverse has a value calculated before 
  message("calculating the inverse")
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  # Sets Inverse to the newly calculated value   
  I #Returns the new Inverse value
}
