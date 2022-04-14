
#enter the number of variables for the equation
var = readline(prompt = "Enter the number of variables for the equations :")
var_num = as.integer(var)
var_loop = 1
#Loop until correct inputs are provided
while(var_loop)
{
  #enter if it is a maximization or minimization problem
  var = readline(prompt = "Is it maximization or minimization problem (min/max)? :")
  if(var == "min" || var == "max")
    var_loop = 0
  else
    print("Please enter min or max")
}

#list initializations

var_coeff <- list()
var_ineq_coeff <- list()
var_ineq <- list(list())
var_ineq_new <- list(list())
var_ineq_sym <- list()
var_art <- list()
var_ineq_rhs <- list()
var_obj_coeff <- list()
var_art_symb <- list()
var_slsu_symb <- list()
var_obj_symb <- list()
var_x_symb <- list()

#variable Initializations
var_max_min = var
var_art_num = 0
var_coeff_num = 0
var_coeff_len = 0
var_neg = -1
M = 10000
if(var_max_min == "max")
  M = -1*M

print("Please provide the coefficients for the variables")

#reading coefficients of the objective function
for(i in 1:var_num)
{
  var = readline(paste("Coefficient ", i, ": ", sep=''))
  var_coeff[i] <- as.integer(var)
  var_symb = paste('X',as.character(i),sep="") #adding variable symbol;s (X1,X2,X3 etc.)
  var_obj_symb[i] = var_symb
}

#storing variable symbols separately for later use
var_x_symb = var_obj_symb 

#number of inequalities as input
var = readline(prompt = "Please enter the number of inequality constraints :")
var_ineq_num <- as.integer(var)

#get inequality equations details
for(i in 1:var_ineq_num)
{
  var_loop = 1
  while(var_loop)
  {
    var_ineq_sym[i] = readline(prompt = "The inequality of the equation (<=,=,>=) :") #enter inequality symbol
    if(var_ineq_sym[i] == "=" || var_ineq_sym[i] == ">=" || var_ineq_sym[i] == "<=")
      var_loop = 0
    else
      print("Please provide the inequality of the equation as (<= , = , >= )")
  }
  
  var = readline(prompt = "Please enter the RHS :") #enter rhs value
  var = as.integer(var) #convert rhsvalue into integer
  var_ineq_rhs[i] = var #store rhs value
  #reverse inequality if rhs is negative
  if(var_ineq_rhs[i] < 0)
  {
    var_ineq_rhs[i] = var*var_neg #reverse rhs
    #reverse symbol
    if(var_ineq_sym[i] == "<=")
      var_ineq_sym[i] = ">="
    else if(var_ineq_sym[i] == ">=")
      var_ineq_sym[i] = "<="
  }
  print("Please enter the coefficients of variables")
  
  #Define Slack / Surplus variables and artificial variables based on the inequality
  if(var_ineq_sym[i] == "<=")
  {
    var_art[i] = 1
    var_coeff_num = var_coeff_num + 1
    var_symb = paste('S',as.character(var_coeff_num),sep="") #include variable symbols
    var_slsu_symb[var_coeff_num] = var_symb
  }
  else if(var_ineq_sym[i] == "=")
  {
    var_art[i] = 2
    var_art_num = var_art_num + 1
    var_symb = paste('A',as.character(var_art_num),sep="")
    var_art_symb[var_art_num] = var_symb
  }
  else if(var_ineq_sym[i] == ">=")
  {
    var_art[i] = 3
    var_art_num = var_art_num + 1
    var_coeff_num = var_coeff_num + 1
    var_symb = paste('S',as.character(var_coeff_num),sep="")
    var_slsu_symb[var_coeff_num] = var_symb
    var_symb = paste('A',as.character(var_art_num),sep="")
    var_art_symb[var_art_num] = var_symb
  }
  
  #enter coefficients of the inequality
  for(j in 1:var_num)
  {
    var = readline(paste("Coefficient ", j, ": ", sep=''))
    var = as.integer(var)
    if(var_ineq_rhs[i] < 0)
      var = var*var_neg
    
    var_ineq_coeff[j] <- var
  }
  var_ineq[i] = list(var_ineq_coeff)
}

var_coeff_len = var_art_num + var_num + var_coeff_num #total number of coefficients
var_obj_symb = c(var_obj_symb,var_slsu_symb,var_art_symb) #all variable symbols (X1,X2,S1,S1,A1 etc.)
var_obj_symb = unlist(var_obj_symb)
coeff_count = 1
art_count = 1
#include slack/surplus and artificial variables into inequality
for(i in 1:var_ineq_num)
{
  var_temp = numeric(var_coeff_num)
  var_art_temp = numeric(var_art_num)
  if(var_art[i] == 1)
  {
    var_temp[coeff_count] = 1
    coeff_count = coeff_count + 1
  }
  else if(var_art[i] == 3)
  {
    var_temp[coeff_count] = -1
    var_art_temp[art_count] = 1
    coeff_count = coeff_count + 1
    art_count = art_count + 1
  }
  else
  {
    var_art_temp[art_count] = 1
    art_count = art_count + 1
  }
  temp_coeff = var_ineq[i]
  rhs_temp = var_ineq_rhs[i]
  temp_coeff = unlist(temp_coeff)
  temp_coeff = c(temp_coeff,var_temp,var_art_temp,rhs_temp)
  var_ineq_new[i] = list(temp_coeff) #Obtain the equation matrix
}

var_obj_coeff = c(var_coeff,numeric(var_coeff_num),rep(list(M),var_art_num)) #Complete objective coefficient with all variable coefficients
print("Objective function coefficients")
print(unlist(var_obj_coeff))

var_cond = 1
var_zj = 0
#Define CB variables based on the number of artificial variables and total number of inequalities
if(var_art_num < var_ineq_num && var_art_num > 0){
  var_cb_symb = c(var_slsu_symb[1:(var_ineq_num-var_art_num)],var_art_symb[1:var_art_num])}else if(var_art_num == var_ineq_num){
    var_cb_symb = var_art_symb[1:var_art_num]}else if(var_art_num == 0)
  var_cb_symb = var_slsu_symb[1:(var_ineq_num-var_art_num)]
  
var_cb_symb = unlist(var_cb_symb)        
var_cb = c(numeric(var_ineq_num-var_art_num),rep(list(M),var_art_num)) #define cb values
var_cb = unlist(var_cb)

print("CB values")
print(var_cb)
print("CB variables")
print(var_cb_symb)

#Form matrix of inequality equations including rhs
matrix_ineq = matrix(unlist(var_ineq_new),ncol = var_coeff_len+1,byrow = TRUE)
var_iter = 0
#loop while condition notsatisfied
while(var_cond)
{
  if(var_iter == 0)
    print("Initial Table")
  else
    cat("Iteration" , var_iter, ": ", sep=' ')
  
  print("Coefficients in inequality with RHS")
  print(matrix_ineq)
  #Compute zj
  for(i in 1:length(var_cb))
  {
    var_zj = var_zj + (var_cb[i]*matrix_ineq[i,0:var_coeff_len+1])
  }
  print("Zj Calculation")
  print(var_zj)
  print("Objective Coefficients")
  print(unlist(var_obj_coeff))
  #cj - zj
  if(var_max_min == "min")
    var_cj_zj = unlist(var_obj_coeff[1:var_coeff_len]) - var_zj[1:var_coeff_len]
  else
    var_cj_zj =  var_zj[1:var_coeff_len] - unlist(var_obj_coeff[1:var_coeff_len])
  
  print("Cj-Zj Calculation")
  print(var_cj_zj)
  var_opt_check = min(var_cj_zj) #Check for optimal condition
  #if condition not met
  if(var_opt_check < 0)
  {
    print("Key Column index")
    key_col_num = which.min(var_cj_zj) #Key column compute
    print(key_col_num)
    key_col = matrix_ineq[1:var_ineq_num,key_col_num]
    print("Key Column")
    print(key_col)
    ratio_val = matrix_ineq[1:var_ineq_num,var_coeff_len+1]/key_col #ratio compute
    print("Ratio Values")
    print(ratio_val)
    inf_ind = grep(Inf,ratio_val)
    if(length(inf_ind)>0)
      ratio_val[inf_ind] = 100000
    for(i in 1:length(ratio_val))
    {
      if(ratio_val[i]<0)
        ratio_val[i] = 100000
    }
    print("Ratio Values for computation")
    print(ratio_val)
    key_row = which.min(ratio_val) #Obtain key row
    print("Key Row")
    print(key_row)
    key_elem = matrix_ineq[key_row,key_col_num] #Key element
    print("Key Element")
    print(key_elem)
    
    var_cb[key_row] = unlist(var_obj_coeff[key_col_num]) #Replace the new cb values with entering variable
    out_var = var_cb_symb[key_row] #leaving variable
    in_var = var_obj_symb[key_col_num]  #entering variable
    out_ind = grep(out_var,var_obj_symb)
    var_cb_symb[key_row] = in_var #entering variable symbol
    
    print("leaving Variable")
    print(out_var)
    print("entering Variable")
    print(in_var)
    print("Out Index")
    print(out_ind)
    
    print("New CB for next iteration")
    print(var_cb)
    print("New CB Variables")
    print(var_cb_symb)
    
    out_ind = -1*out_ind #to remove leaving variable values from matrix
    #matrix_ineq <- matrix_ineq[,out_ind]
    #var_coeff_len = var_coeff_len - 1
    
    matrix_ineq[key_row,0:var_coeff_len+1] = matrix_ineq[key_row,0:var_coeff_len+1]/key_elem #divide entering variable row with key element
    #print(matrix_ineq)
    #Computing values of other rows ex;cluding entering variable (key row)
    for(i in 1:var_ineq_num)
    {
      if(i != key_row) #excluding entering variable
      {
        for(j in 0:var_coeff_len+1)
        {
          matrix_ineq[i,j] = matrix_ineq[i,j] - (key_col[i]*matrix_ineq[key_row,j]) #compute other row values
        }
      }
    }
  
   #print(matrix_ineq)
    matrix_ineq <- matrix_ineq[,out_ind] #remove leaving variable values
    var_obj_symb <- var_obj_symb[out_ind]
    var_obj_coeff <- var_obj_coeff[out_ind]
    var_coeff_len = var_coeff_len - 1 #reduce total coefficient length
    #print(matrix_ineq)
    var_iter = var_iter + 1
    var_zj = 0 #reset zj values for the next iteration
    var_cj_zj = 0
  }

  else #optimal solution reached
  {
    print("Optimal Solution value reached")
    var_opt_sol = var_zj[var_coeff_len+1] #optimal solution from rhs
    #var_sol_coeffs = grep(var_cb_symb,var_x_symb)
    var_sol = matrix_ineq[1:var_ineq_num,var_coeff_len+1] #Variable solution values
    
    print("Optimum Solution Value")
    print(var_opt_sol)
    print("Solution variables")
    print(var_cb_symb)
    print("variable solution")
    print(var_sol)
    var_cond = 0
  }
}

