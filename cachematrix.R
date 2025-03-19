## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL #intializing the inverse matrix
	#function for setting the matrix
	set<-function(y){
		x<<-y
		inv<<-NULL #reset inverse cache into new matrix
	}
	#function to get matrix
	get<-function()x
	#function to set the inverse matrix
	setInverse<-function(inverse)inv<<-inverse
	#function to get the inverse matrix
	getInverse<-function()inv
	#returns the list
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getInverse()
	 # If inverse is already calculated, return it
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	#else computing the data
	data<-x$get()
	inv<-solve(data,...) # Compute inverse using solve()
	x$setInverse(inv)
	inv #returning the data.
}
