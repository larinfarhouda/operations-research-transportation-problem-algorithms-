
var_col = readline(prompt = "Enter the number of col:")
var_col = as.integer(var_col)

var_row = readline(prompt = "Enter the number of row:")
var_row = as.integer(var_row)

mat = matrix(nrow = var_row , ncol = var_col)
opt = mat
A = opt
#matrix values
for (i in 1:var_row) {
  print(paste("enter values of row#",i, ": ", sep=''))
  for (j in 1:var_col) {
    var = readline(paste("vlaue #", j, ": ", sep=''))
    var = as.integer(var)
    mat[i,j] = var 
  }
  
}

################ Step 1: calculation fo reduced matrix ################

#the minimum of each row is calculated and they are subtracted
for(j in 1:ncol(A)){
  for(i in 1:nrow(A)){
    min = min(mat[j,])
    A[j,i] <- mat[j,i]-min
  }
}

#the minimum of each col is calculated and they are subtracted
B <- A
for(i in 1:nrow(A)){
  for(j in 1:ncol(A)){
      min = min(B[,j])
      A[i,j] <- A[i,j]-min
  }
}
reduced <- A

################ Step 2: optimaztion of the problem ################

C <- A
C[,] <- NA
while(TRUE){
  squares_count = 0 
  #row scanning
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      if(A[i,j]==0 && !is.na(A[i,j])){
        if(sum(A[i,]==0,na.rm =TRUE)==1){
          opt[i,j] <- A[i,j]
          squares_count = squares_count + 1
          C[,j] <- A[,j]
          A[,j] <- NA
        }
      }
    }
  }
  

  #col scanning
  #check if we need it 
  for (i in 1:2) {
    
  
  if(squares_count != nrow(A)){
       for (j in 1:ncol(A)) {
         for (i in 1:nrow(A)) {
        if(!is.na(A[i,j]) && A[i,j]==0){
          if(sum(A[,j]==0,na.rm =TRUE)==1){
            opt[i,j] <- A[i,j]
            squares_count = squares_count + 1
            C[i,] <- reduced[i,]
            A[i,] <- NA
          }
        }
      }
    }
    
  }else{
    print("no need for col scanning sol is optimized")
    break;
  }
  }
  if(squares_count == nrow(A)){
    print("u reached optimal matrix")
    break;
  } else{
  #result after row and col scanning
  result <- reduced
  #finding minimum value to form the new matrix
  minval = min(A, na.rm = TRUE)
  #finding intersection points to add "minval" to it
  for (i in 1:nrow(C)) {
    for (j in 1:ncol(C)) {
      if(!is.na(C[i,j])){
        if(sum(is.na(C[i,]))==0 && sum(is.na(C[,j]))==0){
          result[i,j] <- C[i,j]+minval
        }
        }
      }
  }
  
  #subtracting "minval" from undeleted cells
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      if(!is.na(A[i,j])){
          result[i,j] <- A[i,j]-minval
      }
    }
  }
  print(result)
  A <- result
  reduced <- result
  C[,] <- NA
  opt[,] <- NA
  }
  
}

##############printing the optimal solution################

finalsol = matrix(nrow = var_row , ncol = 3)
colnames(finalsol) <- c("job","operator","time")
finalsol[,1] <- 1:var_row
for (i in 1:nrow(opt)) {
  for (j in 1:ncol(opt)) {
    if(!is.na(opt[i,j])){
    finalsol[i,2] <- j
    }
  }
}
for (i in 1:nrow(finalsol)) {
  finalsol[i,3] <- mat[finalsol[i,1],finalsol[i,2]]
}
totaltime = sum(finalsol[,3])   
print(finalsol) 
print(paste("total time : ",totaltime, sep=''))  
