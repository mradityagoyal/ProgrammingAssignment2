## functions for wk3 programming assignment
## creates a matrix that can cache the inverse if calculated. 


## takes a matrix and returns a list of set, get , setInverse and getInverse functions. inverse is cached.

makeCacheMatrix <- function(x = matrix()) {
  # the inverse of the matrix initialized to null.
  inv <- NULL
  
  set <- function(x) {
    mtx <<- x
    inv <<- NULL
  }
  
  get <- function()
    x
  
  setInverse <- function(inverse)
    inv <<- inverse
  
  getInverse <- function() {
    #computation of the inverse should be done here.. and not in the cacheSolve function. I dont like it there.
    #just design pref. i think computing inverse and setting it to inv should be done here. cacheSolve should just call
    #getInverse on this
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    #keeping locig of computing the inverse here instead of inside cacheSolve function allows us to ensure that
    #inv is set computation.. and not just thrown away
    inv <<- solve(x)
    inv
  }
  
  list(
    set = set,
    get = get ,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## takes a matrix x and creates special matrix . returns the inverse of the input matrix. inverse is cached.

cacheSolve <- function(x, ...) {
  specialMatrix <- makeCacheMatrix(x)
  # solving for the inverse and caching it happens inside getInverse() method of the "specialMatrix"
  specialMatrix$getInverse()
}
