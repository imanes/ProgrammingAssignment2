##########################################################################################

#Assigment2 Catching the inverse of a mmatrix

#The first function, makeVector creates a special "vector", which is really a list containing 
#a function to
#A set the value of the vector
#B get the value of the vector
#C set the value of the mean
#D get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        InvMatr <- NULL         #setting solve to NULL as a place holder for a future value
        set <- function (y) {   #defines a function to set the matrixr, x, 
                x <<- y      #x to a new Matrix, y only Within function set x = y and InvMatr = null 
                InvMatr <<- NULL      #resets the solve, InvMatr, to NULL
        }
        get <- function () x    #returns the matrix, x from makeCacheMatrix
        setInv <- function (Inv) InvMatr <<- Inv   # sets solve, InvMatr, to mean (not declared yet)
        getInv <- function () InvMatr              #returns solve, InvMatr (=solve and not declared)
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}
#returns the 'special matrix' containing all of the functions just defined

cacheSolve <- function(x,...){
        InvMatr <- x$getInv ()                       #takes  Mtrx1
        if ( !is.null ( InvMatr ) ) { #looks in makeCacheMatrix the value of InvMatrix (NULL)
                message ( "getting cache data" )        #skips because InvMatr is null
                return ( InvMatr )                      #skips becasue InvMatr is null
        }
        data <- x$get ( )                               #looks in cr$get = matrix values
        InvMatr <- solve ( data, ... )                  #calculcates solve value and call it InvMatr
        x$setInv ( InvMatr )                    #cr$setInv -> set InvMatr function with InvMatr
        InvMatr                                 #shows InvMatr
}

#print("Example to understand what does what is edited in code")

#Matrix1 [4 7, 2 6] 
#INVMatrix1 [0.6 -0.7, -0.2 0.4]


#Mtrx1 <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2, byrow = T)
#Mtrx
#Mtrx2 <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = T)
#solve(Mtrx1)
#a <- makeCacheMatrix(Mtrx1)
#a$get()
#a$getInv()
#cacheSolve(a)
#a$getInv()
#cacheSolve(a)
#a$set(Mtrx2)
#a$getInv()
#cacheSolve(a)
#cacheSolve(a)
#a$get()

