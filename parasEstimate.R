rm(list=ls())
library(quantreg)
library(openxlsx)
library(lme4)
library(caret)
setwd("~/Desktop/CREM_model")

#######Model Fitting
## kind_id: 1-wheat, 2-maize, 3-rice
for(kind_id in 1:3){
  data = read.xlsx("data_annual_input.xlsx",sheet = kind_id)
  x_sam = (1:length(data[,1]))
  y_sam = round(sort(data[,2],decreasing = T)/max(data[,2]),digits = 6)
  min(y_sam)
  par(family="STSongti-SC-Black")
  plot(x_sam,y_sam,pch=20,xlab = "x",ylab = "IRAI",font.axis=2,font.lab=2)
  
  ncolor = rainbow(10)
  sample_size = round(nrow(data)*0.7)
  x_sample_data = matrix(nrow = sample_size,ncol = 10)
  y_sample_data = matrix(nrow = sample_size,ncol = 10)
  for(k in 1:10){
    xx = sample(x_sam,sample_size)
    yy = y_sam[xx]
    x_sample_data[,k] = xx
    y_sample_data[,k] = yy
  }
  k_test = numeric()
  for(k in 1:10){
    xx=x_sample_data[,k]
    yy=y_sample_data[,k]
    fitdata = data.frame(xx,yy)
    result1 = tryCatch(
      {
        fit.sol = nlrq(yy ~ exp(a*xx)+b,data = fitdata,start = list(a=-0.5,b=1))
        lines(xx[order(xx)], predict(fit.sol, newdata=xx)[order(xx)], col=ncolor[k])
        tx = x_sam[-xx]
        ty = y_sam[-xx]
        a = summary(fit.sol)
        ty_pred = exp(a$coefficients[1,1]*tx)+a$coefficients[2,1]
        mse_val = sum((ty-ty_pred)^2)/length(tx)
        rmse_val = RMSE(ty_pred,ty)
        mae_val = MAE(ty_pred,ty)
        r2_val = R2(ty_pred,ty)
        k_test = append(k_test,k)
        write.table(data.frame(k,a$coefficients[1,1],a$coefficients[2,1],mse_val,rmse_val,mae_val,r2_val),
                    paste("fit_exp",kind_id,".txt",sep = ""),col.names = F,row.names = F,append = T)
      },error = function(err){
        print(paste("Error occured at k =",k,"MY_ERROR:  ",err))
      }
    )
  }
  legend("topright",legend = paste("Sample",k_test),col = ncolor[k_test],lty = 1,bty = "n",cex = 0.6)
  
  tx = x_sam
  ty = y_sam
  fit.sol1 = nlrq(ty ~ exp(a*tx)+b,data = fitdata,start = list(a=-0.5,b=1))
  a = summary(fit.sol1)
  a
  ty_pred = predict(fit.sol1, newdata=tx)
  mse_val = sum((ty-ty_pred)^2)/length(tx)
  rmse_val = RMSE(ty_pred,ty)
  mae_val = MAE(ty_pred,ty)
  r2_val = R2(ty_pred,ty)
  par(mai=c(0.9,0.9,0.3,0.3),las=1)
  plot(x_sam,y_sam,pch=20,xlab = "x",ylab = "IRAI",font.axis=2,font.lab=2)
  lines(x_sam, ty_pred, col=2)
  legend("topright",legend = paste(c("MSE","RMSE","MAE","R2"),c(signif(mse_val,digits = 2),signif(rmse_val,digits = 2),signif(mae_val,digits = 2),signif(r2_val,digits = 2)),sep = ": "),
         col = 1,bty = "n",cex = 0.8)
  text(10,0.5,paste("y = exp(",round(a$coefficients[1,1],digits = 3),"x)+",round(a$coefficients[2,1],digits = 3),sep = ""))
  write.table(data.frame(0,a$coefficients[1,1],a$coefficients[2,1],mse_val,rmse_val,mae_val,r2_val),
              paste("fit_exp",kind_id,".txt",sep = ""),col.names = F,row.names = F,append = T)
}
