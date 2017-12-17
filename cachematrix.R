## Set variables x (matrix) and ans (solved matrix) the function's environment 

makeCacheMatrix <- function(x = matrix()) {
  ans <- NULL #set default for ans
  set <- function(y) { #define x and ans within current environment
    x <<- y
    ans <<- NULL
  }
  get <- function() x #returns origninal matrix when called
  solveit <- function(solved) ans <<- solved #Define ans as solved within current environment
  issolved <- function() ans # Check if matrix was solved within cachesolve
  list(set = set, get = get,
       solveit = solveit,
       issolved=issolved)
  
}


## Check to see if matrix has been solved and solve if no data exists

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ans <- x$issolved()
  if(!is.null(ans)) { #Check to see if matrix was solved
    message("getting cached data")
    return(ans) # return solved matrix if it exists
  }
  data <- x$get() #set matrix as data
  ans <- solve(data, ...) #solve
  x$solveit(ans)
  ans
}