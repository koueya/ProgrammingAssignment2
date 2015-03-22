##Below are two functions that are used to create a
##special matrix that stores a numeric matrix and caches its mean.

##The first function, `makeChacheMatrix` creates a special "matrix", which is
##really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the Inverse of the matrix
## 4.  get the Inverse if the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <<- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInv <- function (Inv) i <<- Inv
	getInv <- function() i
	list(set= set, get=get, setInv=setInv, getInv=getInv)
}



##The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the data and sets the value of the inverse in the cache via the `setInv`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInv()
		if(!is.null(i))
		{
		    message("getting cached data")         
		   return(i)
		}
		data <- x$get()
		i <- solve(data)                
                x$setInv(i)
                
                i
}
