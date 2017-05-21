+##The "makeCacheMatrix" function creates a special array object, and then the
  ##"cacheSolve" function calculates the inverse of the array. If the inverse of 
  ##the array has already been computed, it is found in the cache and returns it, 
  ##with no need to recalculate it.
 
  ## This function will: Set the value of the matrix
                      ## Get value of the matrix
                      ## Set value of the inverse
                      ## Get value of the inverse
 
  makeCacheMatrix <- function(x = matrix()) {
    inverse_x<-NULL
    set<-function(y)  {
        x<<-y
        inverse_x<<-NULL
    }

    get<-function() x
    setinverse<-function(inverse) inverse<<-inverse_x
    getinverse<-function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
      }
 
 
   ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    inverse_x<-x$getinverse()
    if(!is.null(inverse_x)) {
      message("getting cached data")
      return(inverse_x)
    }
    data<-x$get()
    inverse_x<-inverse(data, ...)
    x$setinverse(inverse_x)
    inverse_x
    }
    
    
