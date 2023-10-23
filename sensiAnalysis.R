rm(list=ls())
library(quantreg)
library(openxlsx)
library(lme4)
library(caret)
setwd("~/Desktop/CREM_model")

#####Sensitive Analysis
a_list_wheat = numeric()
b_list_wheat = numeric()
a_list_maize = numeric()
b_list_maize = numeric()
a_list_rice = numeric()
b_list_rice = numeric()
a_list_ns = numeric()
b_list_ns = numeric()
## kind_id: 1-wheat, 2-maize, 3-rice, 4-non-staple crop
for(kind_id in 1:4){
  data = read.xlsx("data_annual_input.xlsx",sheet = kind_id)
  x_sam = (1:length(data[,1]))
  y_sam = round(sort(data[,2],decreasing = T)/max(data[,2]),digits = 6)
  sample_size = round(nrow(data)*0.7)
  
  par(mai=c(0.9,0.9,0.3,0.3),las=1)
  plot(x_sam,y_sam,pch=20,xlab = "x",ylab = "IRAI",col="#ef233c",font.axis=2,font.lab=2)
  
  k_test = numeric()
  for(k in 1:2000){
    xx = sample(x_sam,sample_size)
    yy = y_sam[xx]
    fitdata = data.frame(xx,yy)
    result1 = tryCatch(
      {
        fit.sol = nlrq(yy ~ exp(a*xx)+b,data = fitdata,start = list(a=-0.5,b=1))
        lines(xx[order(xx)], predict(fit.sol, newdata=xx)[order(xx)], col="#153997")#a$coefficients[,1]
        tx = x_sam[-xx]
        ty = y_sam[-xx]
        a = summary(fit.sol)
        ty_pred = exp(a$coefficients[1,1]*tx)+a$coefficients[2,1]
        mse_val = sum((ty-ty_pred)^2)/length(tx)
        rmse_val = RMSE(ty_pred,ty)
        mae_val = MAE(ty_pred,ty)
        r2_val = R2(ty_pred,ty)
        k_test = append(k_test,k)
        a_list_rice = append(a_list_rice,a$coefficients[1,1])
        b_list_rice = append(b_list_rice,a$coefficients[2,1])
        write.table(data.frame(k,a$coefficients[1,1],a$coefficients[2,1],mse_val,rmse_val,mae_val,r2_val),"fit_exp221212_rice1.txt",col.names = F,row.names = F,append = T)
      },error = function(err){
        print(paste("Error occured at k =",k,"MY_ERROR:  ",err))
      }
    )
  }
  points(x_sam,y_sam,pch=20,col="#ef233c")
}

rabs_df = data.frame(a_list_wheat,b_list_wheat,a_list_maize,b_list_maize,a_list_rice,b_list_rice,a_list_ns,b_list_ns)
hist_val = apply(rabs_df,2,function(x){round((max(x)-min(x))/mean(x),digits = 2)})
bp_df = t(matrix(hist_val,ncol = 2,byrow = T))
barplot(height = bp_df,  # data for plotting (matrix)
        names.arg = c("Wheat","Maize","Rice","Other Crops"),  # names of bars
        col = c("#023e8a","#7C7B80"),  # filled colors
        border = '#ffffff',   # border colors
        horiz = FALSE,  
        ylim = c(-1, 3), 
        legend.text = c('a', 'b'),  
        args.legend = c(x=2.5,y=3),font.axis=2,font.lab=2,
        beside = TRUE  
)
abline(h=0)
