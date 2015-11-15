## This coding consists 2 functions, 
## 1 makes a specila vecto of functions that do appropriate actiona of making and storing variables
## 2nd brings actions of functions within the special vector to check its value else create the inverse of the matrix
## and then store it in a variable using one of the functions of the special vector.
## using this technique, prevents several runs of computing if the inverse matrix has already been calculated
## and computes only if it is being done for the first time
## this is a conservative method and saves computing time and speeds up execution of functions.



## makecacheMatrix creates a special vector of 4 functions, set, get, setinverse and getinverse, 
## that do appropriate functions and make a vector to store and cache invrse solution

makecacheMatrix <- function(x){
   i <- NULL                               # initializes i to NULL value
   set <- function(y){                     # creates a function named set (takes in y variable)
     x <<- y                               # attributes value of y to x (in the Globalenvironment by use of <<-
     i <<- NULL}                          # i = NULL in Globalenvironment by using <<-
   get <- function() x                    # creates a function named get, that returns value x
   setinverse <- function(inv) i <<- inv  # createsa a function named setinverse, attributes inv to i (globalenv)
   getinverse <- function() i             # returns value of i
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)}
   # creates a list and returns this list when makecacheMatric function is called
   # this list has four elements named set,get,setinverse,getinverse
   # they contain the functions of the same name, as were created above



## This function takes in variables (x,...) and runs them first to check if its value was already solved and cached
## or needs to be solved anew. If cache present then it just pulls value from cache and returns it
## else it will calculate new inverse and stores it in a cache as well as returns the value to be printed


cacheSolve <- function(x,...)             # (x,...) are variables and NOT values
{                                         # eg: (a,b,c,....) where a <- makecacheMatric(1:100)
  i <- x$getinverse()                     # attributes value from x$getinverse() function returns viz i from GlobalEnv
  if (!is.null(i)) {                      # Checks if i value just attributed was Null or Something, if NOT NULL then
    message("getting cached data")        # prints a message "getting cached data" and returns i, 
    return(i)                             # function gets truncated and returns i (does not proceed furhter) 
  }
  data <- x$get()                         # this code runs only if i is NULL, data called X4get(), which brings value x (from Global)
  i <- solve(data, ...)                   # using the value x, it computes inverse value using function solve()
  x$setinverse(i)                         # This value is now stored as i in Global by calling function X$setinverse()
  i
}
