rm(list=ls())
library(quantreg)
library(openxlsx)
library(readxl)    
library(lme4)
library(caret)
setwd("../data/Scenario_Simulation")
output_path = "./CREM_model/"
if(!dir.exists(output_path)){
  dir.create(output_path)
}
setwd(output_path)
file_path = "/Users/lihua/Desktop/zb-sd-pesticide/CPI_model-main/data/情景模拟/3、data_annual/"
dir_list = dir(path = file_path, pattern = ".xlsx",full.names = F)

tau = 0.5
stop_year = 2022
for(fname in dir_list){
  year = as.numeric(gsub("S3_pesticide_usage_|.xlsx","",fname))
  filename_full = paste(file_path,fname,sep = "")
  sheet_names <- readxl::excel_sheets(filename_full)
  num_kind = length(sheet_names)
  
  #######Model Fitting
  ## kind_id: 1-wheat, 2-maize, 3-rice, over 4: non staple cultivars
  a_list = numeric()
  b_list = numeric()
  n_list = numeric()
  ret_list = list()
  for(kind_id in 1:num_kind){
    data = read.xlsx(filename_full,sheet = kind_id)
    x_sam = (1:length(data[,1]))
    y_sam = round(sort(data[,2],decreasing = T)/max(data[,2]),digits = 6)
    
    tx = x_sam
    ty = y_sam
    fitdata = data.frame(tx,ty)
    fit.sol1 = nlrq(ty ~ exp(a*tx)+b,data = fitdata,start = list(a=-0.5,b=1),tau = tau)
    a = coefficients(fit.sol1)
    
    ty_pred = predict(fit.sol1, newdata=tx)
    mse_val = sum((ty-ty_pred)^2)/length(tx)
    rmse_val = RMSE(ty_pred,ty)
    mae_val = MAE(ty_pred,ty)
    r2_val = R2(ty_pred,ty,form = "traditional")
    
    a_list = append(a_list,a[1])
    b_list = append(b_list,a[2])
    n_list = append(n_list,nrow(data))
  }
  para_data = data.frame(`名称`=sheet_names,`a值`=a_list,`b值`=b_list,`样本量`=n_list)
  ret_list[["a"]] = para_data
  
  write.xlsx(ret_list,
             paste("./",year,"_paras.xlsx",sep = ""))
}


