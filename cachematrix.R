# the makeCacheMatrix function "creates" the "matrix":
# in quotes because it is a list containing the options. 
# In another programming languages would be called a "CLASS";
# with: R>   my_cached_matrix <- makeCacheMatrix(normal_R_matrix)
# or  : R>   my_cached_matrix <- makeCacheMatrix()
# we create an "OBJECT" that can be assigned with a new value (set)
# (for example my_cached_matrix$set(another_R_matrix)
# can be listed (obtained, get)(my_cached_matrix$get(),
# can store his "inverted" values  --called from cacheSolve
# and can be listed his inverse.   --called from cacheSolve too.

makeCacheMatrix <- function(x = matrix()) {


  # if no argument given, show info of usage
  if (class(x)!="matrix") {
    stop(paste("Incorrect argument given.\n",
               "Usage: create  an  matrix-like object with ",
               "makeCacheMatrix(matrix)\n  and then use cacheSolve()",
               "\n  to obtain the inverse"))
  }
  if ( is.na(x[1,1])) {
    stop(paste("No argument given.\n",
               "Usage: create  an  matrix-like object with ",
               "makeCacheMatrix(matrix)\n  and then use cacheSolve()",
               "\n  to obtain the inverse"))
  }

  # improvement: if matrix 1*n or n*1 (not square, function 
  if(dim(x)[1]!=dim(x)[2]){
    message("matrix is not square.")
    dx <- dim(x)[1]
    dy <- dim(x)[2]
    if ((dy==1) & (trunc(sqrt(dx))*(trunc(sqrt(dx)))==dx)){
      dim(x) <- c(sqrt(dx),sqrt(dx))
      message (paste("matrix converted to square: [", 
                    dx,"*", dy, "]  ->  [",dim(x)[1],"*", dim(x)[2])," ]")
    } else 
    if ((dx==1) & (trunc(sqrt(dy))*(trunc(sqrt(dy)))==dy)){
      dim(x) <- c(sqrt(dy),sqrt(dy))
      message (paste("matrix converted to square: [", 
                    dx,"*", dy, "]  ->  [",dim(x)[1],"*", dim(x)[2])," ]")
    } else {
      message("CacheMatrix created but will not be inverted")
    }
    
  }
  # end of dim conversion

  inv <- NULL
  set <- function(y) {
    # superasignment.    
    x <<- y
    inv <<- NULL
  }
  get <- function() x   # simply return x. 
  # next lines are the cornerstone of this mechanism.
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv

  # the returned value is a list of functions.
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


# After having created an "object" ( with, for example,
# R>   my_cached_matrix <-makeCacheMarix(x) ) #, we use this function 
# R>   cacheSolve(my_cached_matrix)
# to obtain its inverse. When called for the first time with a 
# "cached matrix" the inverse is calculated and returned. Succesive calls 
# to this function will return the inverse previously calculated (cached) 
cacheSolve <- function(x=NULL, ...) {
  # if no argument given, or not the right class, show info.
  if (( length(x)== 0) | (class(x)!="list")) {
    stop(paste("cacheSolve calculates the inverse of a matrix-like",
               "object\n  created with makeCacheMatrix(matrix)"))
  }
  
  #starts the calculus
  inv <- x$getinverse()
  if(!is.null(inv)) {
      # the message should be removed in real case
      message("      --getting cached data")
    return(inv)
  }
  data <- x$get()
    # the message should be removed in real case
    message("      --calculating inverse")
  inv <- solve(data, ...)
  x$setinverse(inv)

  # Return a matrix that is the inverse of 'x'.
  inv
}
