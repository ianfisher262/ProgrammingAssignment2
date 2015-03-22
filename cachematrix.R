## the makeCacheMatrix has the purpose of
## 1 - creating objects capable of setting the value of a matrix such that it may be cached (set)
## 2 - returning the value of the cached matrix to the console (set)
## 3 - creating objects capable of setting the value of the inverse of a matrix such that it may be cached (setinv)
## 4 - returning the value of the cached inverse of the matrix the console (getinv)

makeCacheMatrix <- function(x = matrix()) {
  
  ## create m in the enviroment defined by makeCacheMatrix and assigns its value as NULL
  m <- NULL
  
  ##The set function resets m to NULL and set the cached x value to the passed in matrix as defined the the user.
  ##both x and m here are assignment the environment created by the makeCacheMatrix rather that that defined by set
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##the get function merely returns the value of x
  get <- function() x
  
  ##the setinv function caches the value of inv to m in the environment created by the makeCacheMatrix function
  setinv <- function(inv) {
    m <<- inv
  }
  
  ##the getinv function merely returns the value of the cached inverse matrix
  
  getinv <- function() m
  
  ##the following code returns a list of the functions previously defined
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##the cacheSolve function has the purpose of checking if the inverse matrix has already been calculated
##and returning its value if it has. if it hasn't, 

cacheSolve <- function(x, ...) {
  
  ##the cached value of the inverse matrix is sought and it's value assigned to m
  m <- x$getinv()
  
  ##if m is not Null, the console returns the text "getting cached data" and then returns the cached value
  ##to the console
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##otherwise, if the value of m is NULL then the value of the matrix as returned by the get function
  ##is sought and assigned to data. the solve() function is then used to return the value of the inverse
  ##matrix and assign it to m. this value is then stored in the cache
  ##m is then returned to the console
  else {
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
  }
  
}

## the code may be tested with the following calls

##create a numeric matrix and assign its value to m (at the global environment)
##  m <- matrix(c(-1, -2, 1, 1), 2,2)

##call the makeCacheMatrix with value m (the user defined matrix) and assign it's value
##to x (at the global environment)
##  x <- makeCacheMatrix(m)

##seek and return the value of x as it exists in the get function - which returns the value 
##of the user defined matrix
##  x$get() 

##call the cacheSolve function with the value x(as it exists at the global environment) and
##assign the returned value to the inv variable, then print the value to the console
##the value returned should be the inverse of the matrix m (as it exists at the global environment)
##  inv <- cacheSolve(x)
##  inv

##this code illustrates what happens when the inverse matrix has already been calculated
##the call should return "getting cached data" then print the value of the inverse matrix to the console
##  inv <- cacheSolve(x)
##  inv
