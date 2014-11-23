## Two functions used for caching and retrieving a matrix inverse,
## sparing us the need to recalculate it for subsequent calculations  

## makeCacheMatrix creates a list object
## cachesolve calls the list object (which contains functions!)


## makeCacheMatrix creates a list object (containing functions), for
## creating a matrix inverse (of the argument matrix) and storing it inside
## the list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {                      # Function that's going to 'x$set'
    x <<- y
    m <<- NULL
  }
  
  get <- function() {x}                     # Function that's going to 'x$get'
  setsolve <- function(solve) {m <<- solve} # Function that's going to 'x$setsolve'
  getsolve <- function() {m}                # Function that's going to 'x$getsolve'
  
  list(set = set,              # Returns a list object, part 'x$set'
       get = get,              # Returns a list object, part 'x$get'
       setsolve = setsolve,    # Returns a list object, part 'x$setsolve'
       getsolve = getsolve)    # Returns a list object, part 'x$getsolve'
}


## cachesolve calls the functions (within the object list from above), returning a
## stored matrix (if it can be found). Now it makes sense that a function
## within the object list has a '<<-' inside, since it will be called within
## this function (in the object list is it only stored as a function; the
## function is never called)

cachesolve <- function(x, ...) {
  m <- x$getsolve()            # Retrieves the would-be stored matrix

  if(!is.null(m)) {            # Checks the would-be matrix  
    print("retrieving cached matrix")
    return(m)                  # Returns fetched matrix from 'x$getsolve',
  }                            # terminating the cachesolve function
  
  mtrx <- x$get()              # Proceeds to solve the matrix inverse,
  m <- solve(mtrx, ...)        # if the would-be matrix turned out to be NULL
  x$setsolve(m)
  m
}

## Thanks to https://class.coursera.org/rprog-009/forum/thread?thread_id=457
## for making this assignment comprehensible.....