rm(list=ls())
library(openxlsx)

out_path = "../data/Scenario_Simulation/CREM_model/"
setwd(out_path)
para_path = "./"
file_path = "/Users/lihua/Desktop/zb-sd-pesticide/CPI_model-main/data/情景模拟/province_pesticide_usage/"
dir_list = dir(path = file_path, pattern = ".xlsx",full.names = F)

## fitted model
fun_c = function(Bs = NA,AR_list = numeric(),a_list = numeric(),b_list = numeric(),n_list = numeric(),f_list=numeric()){
  pred_Bs_list = AR_list * sapply(1:length(n_list),function(x){
    x_tmp = 1:n_list[x]
    return(sum(exp(a_list[x] * x_tmp) + b_list[x]))
    })
  dta_Bs = (Bs - sum(pred_Bs_list))*f_list
  optim_c = dta_Bs / (AR_list*n_list)
  if(length(which(f_list==0))>0){
    optim_c[which(f_list==0)] = 0
  }
  return(optim_c)
}

for(fname in dir_list){
  year = as.numeric(gsub("new_province_pesticide_usage_|.xlsx","",fname))
  filename_full = paste(file_path,fname,sep = "")
  data_adj = read.xlsx(filename_full,sheet = 1)
  
  para_file = paste(para_path,year,"_paras.xlsx",sep = "")
  para_data = read.xlsx(para_file)
  #####Model adjusting
  a_list = para_data$`a值`
  b_list = para_data$`b值`
  n_list = para_data$`样本量`
  
  A_staple_ori = apply(data_adj[,3:(3+nrow(para_data)-1)],1,FUN = function(x){
    temp_X = x * sapply(1:length(n_list),function(y){
      x_tmp = 1:n_list[y]
      return(sum(exp(a_list[y] * x_tmp) + b_list[y]))
    })
    return(temp_X)
  })
  colnames(A_staple_ori) = data_adj[,1]
  
  ret_list = list()
  ret_bc = data.frame()
  for(s in 1:nrow(data_adj)){
    B_s = data_adj[s,2]
    AR_list = data_adj[s,3:(3+nrow(para_data)-1)]
    # Allocate according to usage
    f_list = A_staple_ori[,s]/sum(A_staple_ori[,s])
    dA_ori = sum(A_staple_ori[,s])-B_s
    
    c_optim = fun_c(Bs = B_s,AR_list = AR_list,a_list = a_list,b_list = b_list,n_list = n_list,f_list = f_list)
    tmp_df = as.data.frame(matrix(as.numeric(c_optim)+b_list,nrow = 1))
    colnames(tmp_df) = para_data$`名称`
    ret_bc = rbind(ret_bc,tmp_df)
  }
  ret_bc = cbind(data.frame(province=data_adj$province),ret_bc)
  ret_list[["a"]] = para_data
  ret_list[["b+c"]] = ret_bc
  write.xlsx(ret_list, paste(year,"_paras.xlsx",sep = ""))
}


