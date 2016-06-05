## Programming Assignment #2 <Week 3>, <ahundredmonkeys>

## This module takes a square matrix as input and checks whether a cached value of the inverse of this
## matrix already exists. If inverse is cached in memory, it is retrieved and sent to output, else
## the inverse is computed and sent to output.

## makeCacheMatrix function creates list of functions that are used to set/retrieve the matrix 
## and to set/retrieve its inverse

makeCacheMatrix <- function(x = matrix())
{

  ## initialize the inverse
  inv_mat <- NULL
  
  ## function to set the argument
  set <-function(y)
  {
    x<<-y
    inv_mat<<-NULL
  }
  
  ## function to retrieve the argument
  get <-function() x
  
  ## function to designate set_inv as the function to calculate the inverse of matrix
  set_inv <- function(solve) inv_mat <<- solve
  
  ## function to designate get_inv as the function to retrieve the inverse of matrix
  get_inv <- function() inv_mat
  
  ## collect the functions in  a list
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
  
}

## This section checks whether matrix inverse has been cached and if so retrieves from cache 

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  
  ##retrieve the output of get_inv function
  inv_mat<-x$get_inv()
  
  ## if cached data exists, retrieve and print the inverse
  if(!is.null(inv_mat))
  {
    message("getting matrix inverse from cache")
    return(inv_mat)
  }
  
  ## else solve for the inverse and print output
  
  
  data <-x$get()
  
  inv_mat<-solve(data, ...)
  
  x$set_inv(inv_mat)
  
  ## return inverse
  inv_mat
    
}
