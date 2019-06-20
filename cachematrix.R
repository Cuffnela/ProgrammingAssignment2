## 
## 

## Function makeCacheMatrix takes in a matrix and returns a list with four elements
##
## Returned functions in list:
## set: assigns the value of the matrix
## get: gets the cached matrix
## setInverse: uses the solve function to set inverse
## getInverse: gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<- function(y){
        x<<-y
        m <<-NULL
    }
    get<-function()x
    setInverse<-function(solve)m<<-solve
    getInverse<-function()m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Function cacheSolve finds the inverse of a matrix. 
##
## First checks to see if the mean has already been calculated.
##
## If the inverse is already calculated pulls the inverse from the cache 
## and skips computation.
##
## If not calculated. Runs the computation and caches the inverse via
## setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
}
