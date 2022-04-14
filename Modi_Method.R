
####### inputs ########

#Input matrix where last row is the Demand and last column is the Supply

var_col = readline(prompt = "Enter the number of col:")
var_col = as.integer(var_col)

var_row = readline(prompt = "Enter the number of row:")
var_row = as.integer(var_row)


#matrix
matx = matrix(nrow = var_row+1 , ncol = var_col+1)


#matrix values
for (i in 1:var_row) {
  print(paste("enter values of row#",i, ": ", sep=''))
  for (j in 1:var_col) {
    var = readline(paste("vlaue #", j, ": ", sep=''))
    var = as.integer(var)
    matx[i,j] = var 
  }
  
}

#supply values
print("enter supply values:")
for (i in 1:var_row) {
  var = readline(paste("vlaue #", i, ": ", sep=''))
  var = as.integer(var)
  matx[i,var_col+1] = var 
}

#demand values
print("enter demand values:")
for (i in 1:var_col) {
  var = readline(paste("vlaue #", i, ": ", sep=''))
  var = as.integer(var)
  matx[var_row+1,i] = var 
}

#filling the last cell in last col and last row
matx[var_row+1,var_col+1] = 0

#convert to data frame
matx = data.frame(matx)


matx = inputmat 

################ calculations of initial sol. #################

#making sure there is no empty cells
if(sum(is.na(matx))>0)
  print("the matrix has empty cells")

#new matrix without supply and demand 
Alloc_Matrix=matx[-nrow(matx),-ncol(matx)]
Alloc_Matrix[,]=0
#matrix for modi method
modi_matrix = matx
modi_matrix[,] =0
colnames(modi_matrix)[ncol(modi_matrix)] <- "u"
rownames(modi_matrix)[nrow(modi_matrix)] <- "v"
#making U and v values initially = NA
modi_matrix[nrow(modi_matrix),] <- NA
modi_matrix[,ncol(modi_matrix)] <- NA

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
  modi_matrix[tr,tc]= matx[tr,tc]
  
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
 





######################## Modi method ############################

#find the maximum allcocation for rows
max_r = 0
max_r_indx = 0
for (i in 1:var_row) {
  alloc_num =0
  for(j in 1:var_col){
  if (modi_matrix[i,j]>0){
    alloc_num = alloc_num + 1
  }
  }
  if (max_r < alloc_num){
    max_r = alloc_num
    max_r_indx = i 
  }
}


#find the maximum allcocation for coulnms
max_c = 0
max_c_indx = 0
for (j in 1:var_col) {
  alloc_num =0
  for(i in 1:var_row){
    if (modi_matrix[i,j]>0){
      alloc_num = alloc_num + 1
    }
  }
  if (max_c < alloc_num){
    max_c = alloc_num
    max_c_indx = j 
  }
}


# finding starting point
start = 0

if (max_r > max_c){
  start = max_r
  print(paste("starting from u",max_r_indx, " = 0 ", sep=''))
  modi_matrix[max_r_indx,ncol(modi_matrix)] = 0
  
  #checking the row that has the starting point and setting its v values
  for(j in 1:var_col){
    if (modi_matrix[max_r_indx,j] > 0){
      modi_matrix[nrow(modi_matrix),j] = modi_matrix[max_r_indx,j]
    }
  }
}else{
  start = max_c
  print(paste("starting from v",max_c_indx, " = 0 ", sep=''))
  modi_matrix[nrow(modi_matrix),max_c_indx] = 0
  
  #checking the col that has the starting point and setting its u values
  for(i in 1:var_row){
    if (modi_matrix[i,max_c_indx] > 0){
      modi_matrix[i,ncol(modi_matrix)] = modi_matrix[i,max_c_indx]
    }
  }}

#calculating cij = ui + vj for the rest of cells
while (sum(is.na(modi_matrix))>0) {
  
#*******u based calculation***********
for (i in 1:nrow(modi_matrix)) {
  if(is.na(modi_matrix[i,ncol(modi_matrix)])){
  for (j in 1:var_col) {
      if(!is.na(modi_matrix[nrow(modi_matrix),j])){
        if(modi_matrix[i,j]>0){
          modi_matrix[i,ncol(modi_matrix)] = modi_matrix[i,j] - modi_matrix[nrow(modi_matrix),j]
        }
      }
    }
  }
}

#*******v based calculation*************
for (j in 1:ncol(modi_matrix)) {
  if(is.na(modi_matrix[nrow(modi_matrix),j])){
    for (i in 1:nrow(modi_matrix)) {
      if(!is.na(modi_matrix[i,ncol(modi_matrix)])){
        if(modi_matrix[i,j]>0){
          modi_matrix[nrow(modi_matrix),j] = modi_matrix[i,j] - modi_matrix[i,ncol(modi_matrix)]
        }
      }
    }
  }
}
}

################### second step ####################
#calculating dij for unoccupied cells

#preparing new matrix with only unoccupied cell 
modi2_matrix = matx[-nrow(matx),-ncol(matx)]
modi2_matrix = modi2_matrix - modi_matrix[-nrow(modi_matrix),-ncol(modi_matrix)]


for(i in 1: nrow(modi2_matrix)){
  for(j in 1: ncol(modi2_matrix)){
    if(modi2_matrix[i,j] == 0){
      modi2_matrix[i,j] <- NA
    }
  }
}

### finding dij
## dij = u + v - c
for(i in 1:nrow(modi2_matrix)){
  for(j in 1:ncol(modi2_matrix)){
    if(!is.na(modi2_matrix[i,j])){
      modi2_matrix[i,j] = (modi_matrix[i,ncol(modi_matrix)] + modi_matrix[nrow(modi_matrix),j]) - modi2_matrix[i,j] 
    }
  }
}

##check for optimality 
optimal = 0
for(i in 1:nrow(modi2_matrix)){
  for(j in 1:ncol(modi2_matrix)){
    if(!is.na(modi2_matrix[i,j])){
      if(modi2_matrix[i,j]>=0){
        optimal = optimal +1
    }
  }
  }
}


##checking if we need a second iteration
if(optimal >= 0){
  print("solution is not optimal") 
  
  #finding minimum dij
  mindij = min(modi2_matrix, na.rm = TRUE)
  
}else{
  print("solution is optimal")
}
