## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Try run as below
## x <- rbind(c(1, 5), c(5, 1))
## cache <- makeCacheMatrix(x)
## cache$get()
##            [,1] [,2]
##    [1,]    1    5
##    [2,]    5    1
## cacheSolve(cache)
##        [,1]        [,2]
##  [1,] -0.04166667  0.20833333
##  [2,]  0.20833333 -0.04166667


makeCacheMatrix <- function(x = matrix()) {
  val <- NULL
  
  set <- function(y) {
    x <<- y
    val <<- NULL
  }
  
  get <- function() {
    return(x)
  }
  
  set_inverse <- function(solve){
    val <<- solve
  }
  
  get_inverse <- function(){
    return(val)
  }
  
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  
  if(!is.null(inverse)){    
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data)
  
  x$set_inverse(inverse)
  
  return(inverse)
}


