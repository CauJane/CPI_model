rm(list=ls())
library(quantreg)
library(openxlsx)
library(lme4)
library(caret)
setwd("~/Desktop/CPI_model")

#####Sensitive Analysis
a_list = matrix(nrow = 2000,ncol = 12)
b_list = matrix(nrow = 2000,ncol = 12)

## kind_id: 1-wheat, 2-rice, 3-maize, 0ver 4: non staple cultivars
for(kind_id in 1:12){
  data = read.xlsx("data_annual_input.xlsx",sheet = kind_id)
  x_sam = (1:length(data[,1]))
  y_sam = round(sort(data[,2],decreasing = T)/max(data[,2]),digits = 6)
  sample_size = round(nrow(data)*0.7)
  
  png(paste("./fig_sensAnaly/cultivar_sensi",kind_id,".png",sep = ""),res = 300,width = 1500,height = 1200)
  par(mai=c(0.8,0.8,0.2,0.2),las=1)
  plot(x_sam,y_sam,pch=20,xlab = "x",ylab = "RIs",col="#ef233c",font.axis=2,font.lab=2)
  
  k_test = numeric()
  a_list_tmp = numeric()
  b_list_tmp = numeric()
  for(k in 1:2000){
    xx = sample(x_sam,sample_size)
    yy = y_sam[xx]
    fitdata = data.frame(xx,yy)
    result1 = tryCatch(
      {
        fit.sol = nlrq(yy ~ exp(a*xx)+b,data = fitdata,start = list(a=-0.5,b=1))
        tx = x_sam
        ty = y_sam
        a = summary(fit.sol)
        ty_pred = exp(a$coefficients[1,1]*tx)+a$coefficients[2,1]
        mse_val = sum((ty-ty_pred)^2)/length(tx)
        rmse_val = RMSE(ty_pred,ty)
        mae_val = MAE(ty_pred,ty)
        r2_val = R2(ty_pred,ty)
        if(r2_val>=0.8){
          lines(xx[order(xx)], predict(fit.sol, newdata=xx)[order(xx)], col="#153997")#a$coefficients[,1]
          k_test = append(k_test,k)
          a_list_tmp = append(a_list_tmp,a$coefficients[1,1])
          b_list_tmp = append(b_list_tmp,a$coefficients[2,1])
        }else{
          a_list_tmp = append(a_list_tmp,NA)
          b_list_tmp = append(b_list_tmp,NA)
        }
      },error = function(err){
        a_list_tmp = append(a_list_tmp,NA)
        b_list_tmp = append(b_list_tmp,NA)
        print(paste("Error occured at k =",k,"MY_ERROR:  ",err))
      }
    )
  }
  points(x_sam,y_sam,pch=20,col="#ef233c")
  dev.off()
  a_list[,kind_id] = a_list_tmp
  b_list[,kind_id] = b_list_tmp
}
save(a_list,b_list,file = "env_sensi.RData")

hist_val_a = apply(a_list,2,function(x){round((max(x,na.rm = T)-min(x,na.rm = T))/mean(x,na.rm = T),digits = 2)})
hist_val_b = apply(b_list,2,function(x){round((max(x,na.rm = T)-min(x,na.rm = T))/mean(x,na.rm = T),digits = 2)})
bp_df = rbind(hist_val_a,hist_val_b)
colnames(bp_df) = c("Wheat","Rice","Maize","Vegetables","Temperate fruits","Tea","Fibre crops",
                    "Oil crops","Groundnut","Soybean","Pulses","Tuber crops")
png(paste("./fig_sensAnaly/hist_sensi.png",sep = ""),res = 300,width = 1500,height = 1200)
k = barplot(height = bp_df,  # data for plotting (matrix)
            col = c("#023e8a","#7C7B80"),  # filled colors
            border = '#ffffff',   # border colors
            las=2,xaxt="n",
            font.axis=2,font.lab=2,cex.axis = 0.6,
            beside = TRUE  
)
text(x=apply(k,2,mean)+1,y=-30,srt=30,xpd=T,labels = colnames(bp_df),adj = 1,cex = 0.6)
legend("topleft",legend = c("a","b"),fill =c("#023e8a","#7C7B80") ,bty = "n",cex = 0.7)
abline(h=0)
dev.off()
