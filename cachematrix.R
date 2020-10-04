## Put comments here that give an overall description of what your
## functions do
## Oh man!!!
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          #set i to null for a placeholder for future value
          i <- NULL
          #set x to y and reset inverse i to null     
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
          get <- function() x #return x
          setinverse <- function(inverse) i <<- inverse #set the inverse i to inverse
          getinverse <- function() i #return the inverse
          list(set = set,
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)#return the special vector containing all of the special functions defined above
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getinverse() #get inverse of x and set to i
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }#if i is not null return i
  data <- x$get()#get x
  i <- solve(data, ...)#get new value of inverse
  x$setinverse(i)#set this new value to inverse
  i#return value is the inverse
}
