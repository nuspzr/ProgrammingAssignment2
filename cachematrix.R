## Matrix inversion is usually a costly computation.
## Instead of computing inversions repeatedly
## We can write functions that cache the inversed matrices

## The first function: makeCacheMatrix()
## This function creates a special "matrix"object that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse<-NULL #initialize a NULL object
 
 set<-function(y){ 
   x<<-y #assign value to x.
   inverse<<-NULL #Then reset inverse back to NULL.
 } 
 
 get<-function() x #Retrieve the matrix.
 setinv<-function(inv) inverse<<- inv #Set a new value to inverse.
 getinv<-function()inversee #Retrieve the value of inverse.
 
 list(set=set,get=get
      setinv=setinv
      getinv=getinv) # Returns a list of functions created above.
}


## The second function: cacheSolve()
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 inverse<-x$getinv() #Retrieve the latest value of inverse
 
 if(!is.null(inverse)){
   message ("getting cached data")
   return(inverse)
 } # Return the cached value if the inverse has been calculated.
 
 data<-x$get() #Read the matrix whose inverse has not been calculated
 inverse<-solve(data,...) #Calculate the inverse
 x$setinv(inverse) #Cache the inverse value
 inverse #Return the inverse value just calculated
}
