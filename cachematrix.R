#Function for Lexical Scoping 

makeCacheMatrix <- function(x = matrix()) 
{
  #inv is initalized to null
  inv <- NULL
  
  #Setting the value of y to x
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  #Get the value of x
  get <- function() 
  {
    x
  }
  
  #Setting the inverse 
  setinv <- function(i) 
  {
    inv <<- i
  }
  #Getting the inverse
  getinv <- function()
  {
    inv
  }
  #List of options to be done 
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
   }
  #cachesolve is used to find the inverse of the matrix
  cacheSolve <- function(x, ...)
  {
  
     inv <- x$getinv()
     #Checking for null value 
     if(!is.null(inv)) 
     {
    
        message("Try to get cached inverse value")
        return(inv)
      }
  
  #Setting the matrix
  matr <- x$get()
  #Returing the matrix in inverse form
  inv <- solve(matr, ...)
  #Setting the inverse matrix
  x$setinv(inv)
  #Returning the inverse matrix
  return(inv)
}