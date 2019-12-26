##Reading input as vectors
x <- c(5, 5, 5, 5)
#Printing out the vector
x
#Performing vector arithmetic
x <- x+5

# Convert vector to logical vector
as.logical(x)
x

yvec <- vector("logical",length = 10)
yvec <- as.logical(yvec+TRUE)
yvec

# Playing with lists
zlist <- list(1:10, "Happy","d","a","y","s")
zlist

#Martices
#Normal creation
mat1 <- matrix(1:10, nrow = 5,ncol = 5)
dim(mat1)
mat1

#From vector
vec1 <- vector("numeric", length = 10)
dim(vec1) <- c(5,2)
vec1

#rbind and c bind
row1 <- c(5,5,5)
row2 <- c(20,25,50)

mat3 <- rbind(row1,row2)
dim(mat3)
mat3




