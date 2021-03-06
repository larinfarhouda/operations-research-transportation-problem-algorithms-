####### inputs ########

#Input matrix where last row is the Demand and last column is the Supply

var_col = readline(prompt = "Enter the number of col:")
var_col = as.integer(var_col)

var_row = readline(prompt = "Enter the number of row:")
var_row = as.integer(var_row)


#matrix
mat = matrix(nrow = var_row+1 , ncol = var_col+1)


#matrix values
for (i in 1:var_row) {
  print(paste("enter values of row#",i, ": ", sep=''))
  for (j in 1:var_col) {
    var = readline(paste("vlaue #", j, ": ", sep=''))
    var = as.integer(var)
    mat[i,j] = var 
  }
  
}

#supply values
print("enter supply values:")
for (i in 1:var_row) {
  var = readline(paste("vlaue #", i, ": ", sep=''))
  var = as.integer(var)
  mat[i,var_col+1] = var 
}

#demand values
print("enter demand values:")
for (i in 1:var_col) {
  var = readline(paste("vlaue #", i, ": ", sep=''))
  var = as.integer(var)
  mat[var_row+1,i] = var 
}

#filling the last cell in last col and last row
mat[var_row+1,var_col+1] = 0

#convert to data frame
matx = data.frame(mat)



################ calculations #################

#making sure there is no empty cells
if(sum(is.na(matx))>0)
  print("the matrix has empty cells")

#new matrix without supply and demand 
Alloc_Matrix=matx[-nrow(matx),-ncol(matx)]
Alloc_Matrix[,]=0
tr=1
tc=1
Total_Cost=0
Total_alloc=0
colnames(matx)[ncol(matx)]="Supply"

while(sum(matx[nrow(matx),]) != 0 & sum(matx[,ncol(matx)]) != 0)
{
  min_curr=min(matx[tr,ncol(matx)],matx[nrow(matx),tc])
  matx[tr,ncol(matx)]=matx[tr,ncol(matx)] - min_curr
  matx[nrow(matx),tc]=matx[nrow(matx),tc] - min_curr
  Alloc_Matrix[tr,tc]= min_curr 
  Total_Cost=Total_Cost+(min_curr*matx[tr,tc])
  
  print(Alloc_Matrix)
  
  if(matx[nrow(matx),tc]==0)
  {
    tc=tc+1
  }else if(matx[tr,ncol(matx)]==matx[nrow(matx),tc])
  {
    tr=tr+1
    tc=tc+1
  }else{
    tr=tr+1
  }
  matx[nrow(matx),ncol(matx)]=sum(matx$Supply[-nrow(matx)])
  Total_alloc=Total_alloc+1
}

output=list()
output$Alloc_Matrix=Alloc_Matrix
output$Total_Cost=Total_Cost

#If Supply and Demand are not same
if(sum(matx[nrow(matx),]) != 0 && sum(matx[,ncol(matx)]) != 0){ 
  output$Dummy_demand=sum(matx[nrow(matx),])
  output$Dummy_supply=sum(matx[,ncol(matx)])
}

if(Total_alloc < (nrow(Alloc_Matrix) + ncol(Alloc_Matrix)-1))
  warning("Degenracy in Transporation Problem Occurred")

print(output)
 
