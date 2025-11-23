rm(list=ls())
library(openxlsx)
library(car)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(showtext)
showtext_auto(enable = TRUE)
font_add('Songti', 'Songti.ttc')

setwd("../data/")
###
#---- Part 1: Temporal Trend Quantification of Registered Pesticides in VMP and knapsack modes ----
###
files_list = c("1_pesticide_input_data_VMP_mode.xlsx",
               "2_pesticide_input_data_knapsack_mode.xlsx")
mode_list = c("VMP","Knapsack")
typePes_list = c("Insecticides","Fungicides","Herbicides")
bg_cols = brewer.pal(12, "Paired")[c(2,4,10)]
ret_list = list()
rm_list = list()

for(f_id in 1:length(files_list)){
  fname = files_list[f_id]
  data_ori = read.xlsx(fname,sheet = 1)
  dataType = read.xlsx(fname,sheet = 2)
  p = ggplot()+
    xlim(0, 25) + ylim(ifelse(f_id==2,-4,-1), 10)+
    theme_bw()+
    labs(x="registered duration (yrs)",y="Exceedance Index",title = "")+
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),panel.grid.major = element_blank())
  
  p_anno = ggplot(data = data.frame()) + geom_point()+
    xlim(0, 1) + ylim(0, 1)+
    theme_bw()+
    theme(panel.background = element_blank(),panel.border = element_blank(),
          axis.text = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),panel.grid.major = element_line(colour = NA))
  
  # Adjust the calculated pesticide type, 
  # if you don't need to filter data, change it directly to typePes = NA
  for(k in 1:length(typePes_list)){
    typePes = typePes_list[k]
    list_name = paste(mode_list[f_id],typePes,sep = "-")
    # Whether or not mean processing is required: T is Yes，F means No
    mean_lab = T
    
    ## filtering data
    if(!is.na(typePes)){
      pesticides_list = dataType$`Pesticides`[dataType$`Type_(adjusted_version)`==typePes]
      data = data_ori[which(data_ori$`Pesticides`%in%pesticides_list),]
    }else{
      data = data_ori
    }
    ## mean processing
    if(mean_lab){
      data = aggregate(`Exceedance_index`~`Pesticides`+`Registration_duration`,data = data,FUN = mean)
    }
    
    sam_x = data$`Registration_duration`
    sam_y = data$`Exceedance_index`
    
    tx = seq(1,max(sam_x))
    fitdata = data.frame(x=sam_x,y=sam_y)
    if(k %in% c(1,2) & f_id == 2){
      fit.sol = lm(y~1+x+I(x^2),data = fitdata)
      png(paste0(mode_list[f_id],"-",typePes,"_outliers.png"),res = 300,width = 1800,height = 1200)
      influencePlot(fit.sol)
      dev.off()
      influnceData = influencePlot(fit.sol)
      sele_id = which(influnceData$StudRes>2)
      if(length(sele_id)>0){
        rm_id = as.numeric(rownames(influnceData)[sele_id])
        rmData = cbind(data[rm_id,],influnceData[sele_id,])
        rm_list[[list_name]] = rmData
        fitdata = fitdata[-rm_id,]
      }
    }
    # Quadratic functions
    fit.sol = lm(y~1+x+I(x^2),data = fitdata)
    tmp_fit = summary(fit.sol)
    formula_coef = coef(fit.sol)
    names(formula_coef) = c("c","b","a")
    
    # Calculate the R squared statistic(R2)
    y_pred = predict(fit.sol,newdata = fitdata)
    CI_pred = predict(fit.sol,newdata = fitdata, interval = "confidence")
    CI_pred = cbind(fitdata,CI_pred)
    
    y = fitdata$y
    r2_val = tmp_fit$r.squared
    p_val = pf(tmp_fit$fstatistic[1],tmp_fit$fstatistic[2],tmp_fit$fstatistic[3],lower.tail = F)
    p_val = round(p_val,digits = 2)
    p_val = ifelse(p_val==0,"<0.01",paste("=",p_val,sep = ""))
    mae_val = mean(abs(y_pred-y))
    mse_val = mean((y_pred-y)^2)
    annotate_p = p_anno + annotate("text",x=0.16,y=0.25*k+0.1,label=typePes_list[k],hjust=0)+
      annotate("text",x=0.5,y=0.25*k,size=2.5,
               label=bquote(y==.(round(formula_coef["a"],digits = 3))~x^2~+.(round(formula_coef["b"],digits = 3))~x+.(round(formula_coef["c"],digits = 3))))+
      annotate("text",x=0.5,y=0.25*k-0.05,size=2.5,
               label=bquote('p value'~.(p_val)~', MAE'==.(round(mae_val,digits = 2))~', MSE'==.(round(mse_val,digits = 2))))
    
    if(formula_coef["b"]<0){
      if(formula_coef["c"]<0){
        annotate_p = p_anno + annotate("text",x=0.16,y=0.25*k+0.1,label=typePes_list[k],hjust=0)+
          annotate("text",x=0.5,y=0.25*k,size=2.5,
                   label=bquote(y==.(round(formula_coef["a"],digits = 3))~x^2~-.(round(abs(formula_coef["b"]),digits = 3))~x-.(round(abs(formula_coef["c"]),digits = 3))))+
          annotate("text",x=0.5,y=0.25*k-0.05,size=2.5,
                   label=bquote('p value'~.(p_val)~', MAE'==.(round(mae_val,digits = 2))~', MSE'==.(round(mse_val,digits = 2))))
      }else{
        annotate_p = p_anno + annotate("text",x=0.16,y=0.25*k+0.1,label=typePes_list[k],hjust=0)+
          annotate("text",x=0.5,y=0.25*k,size=2.5,
                   label=bquote(y==.(round(formula_coef["a"],digits = 3))~x^2~-.(round(abs(formula_coef["b"]),digits = 3))~x+.(round(formula_coef["c"],digits = 3))))+
          annotate("text",x=0.5,y=0.25*k-0.05,size=2.5,
                   label=bquote('p value'~.(p_val)~', MAE'==.(round(mae_val,digits = 2))~', MSE'==.(round(mse_val,digits = 2))))
      }
    }
    p_anno = annotate_p +
      geom_segment(data = data.frame(x=0,y=0.25*k+0.1,xend=0.15,yend=0.25*k+0.1),aes(x=x,y=y,xend=xend,yend=yend),color=bg_cols[k],lty=2)+
      geom_point(data = data.frame(x=0.075,y=0.25*k+0.1),aes(x=x,y=y))+
      geom_ribbon(data = data.frame(x=c(0,0.15),ymin=c(0.25*k+0.05,0.25*k+0.05),ymax=c(0.25*k+0.15,0.25*k+0.15)),
                  aes(x=x,ymin=ymin,ymax=ymax),color=bg_cols[k],fill=bg_cols[k],alpha=0.55)
    
    p <- p +
      geom_point(data = fitdata,aes(x=x,y=y),show.legend = F,color=bg_cols[k])+
      stat_smooth(data = fitdata,aes(x=x,y=y),method = lm,formula = y~1+x+I(x^2),
                  color=bg_cols[k],fill=bg_cols[k],lty=2)
    
    ret_list[[list_name]] = list(fitted_model = fit.sol, formula_coef = formula_coef, CI_pred = CI_pred, r2_val = r2_val, plotFig = p)
  }
  png(paste(mode_list[f_id],".png",sep = ""),res = 300,width = 1800,height = 1200)
  print(ggarrange(p, p_anno, 
                  ncol = 2, nrow = 1,
                  widths = c(2,1)))
  dev.off()
}
write.xlsx(rm_list,"Identified_outliers.xlsx")

###
#---- Part 2: Temporal Trend Quantification of forbidden Pesticides ----
###
data = read.xlsx("3_forbidden_pesticides.xlsx",sheet = 1)

# Whether mean normalization is required: T indicates yes, F indicates no.
mean_lab = T

## mean normalization
if(mean_lab){
  data = aggregate(`Exceedance_index`~`Pesticides`+`Registration_durations`,data = data,FUN = mean)
}

sam_x = data$`Registration_durations`
sam_y = data$`Exceedance_index`
y = sam_y/(max(sam_y)+0.01)
y = log(y/(1-y))

tx = seq(1,max(sam_x))
fitdata = data.frame(x=sam_x,y=y)
# fitting the S-shaped curve: logical regression
fit.sol = lm(y ~ x,data = fitdata)
tmp_fit = summary(fit.sol)
formula_coef = coef(fit.sol)
names(formula_coef) = c("b","a")

# calculate the statistic
y_pred_tmp = predict(fit.sol,newdata = fitdata)
CI_pred = predict(fit.sol,newdata = fitdata, interval = "confidence")
fitdata$sam_y = sam_y
CI_pred = cbind(fitdata,CI_pred)
# predicted value
y_pred = max(sam_y)*exp(y_pred_tmp)/(1+exp(y_pred_tmp))
CI_pred$fit = max(sam_y)*exp(CI_pred$fit)/(1+exp(CI_pred$fit))
CI_pred$lwr = max(sam_y)*exp(CI_pred$lwr)/(1+exp(CI_pred$lwr))
CI_pred$upr = max(sam_y)*exp(CI_pred$upr)/(1+exp(CI_pred$upr))

y = fitdata$y
mae_val = mean(abs(y_pred-y))
mse_val = mean((y_pred-y)^2)
p <- ggplot(data = CI_pred,aes(x=x,y=sam_y))+
  geom_point(show.legend = F,color="black")+
  geom_ribbon(aes(x = x,ymin=lwr,ymax=upr),color="lightgrey",alpha=0.35)+
  geom_line(aes(x = x,y = fit),color="red") +
  annotate("text",x=quantile(tx,0.8),y=quantile(sam_y,0.9)+1,
           label=bquote(y==.(round(max(sam_y),digits = 3))~frac(
             e^(.(round(formula_coef["a"],digits = 3))~x-.(round(abs(formula_coef["b"]),digits = 3))),
             1+e^(.(round(formula_coef["a"],digits = 3))~x-.(round(abs(formula_coef["b"]),digits = 3))))))+
  annotate("text",x=quantile(tx,0.75),y=quantile(sam_y,0.85),size=3,
           label=bquote('MAE'==.(round(mae_val,digits = 2))~', MSE'==.(round(mse_val,digits = 2))))+
  theme_bw()+
  labs(x="forbidden duration (yrs)",y="Exceedance Index",title = "")+
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank())
ret_list[["forbidden"]] = list(fitted_model = fit.sol, formula_coef = formula_coef, CI_pred = CI_pred, r2_val = r2_val, plotFig = p)

png("forbidden_pesticides.png",res = 300,width = 1500,height = 1200)
p
dev.off()

save(ret_list,file = "fitted_model.RData")

###
#---- Part 3: Predict application rates of diverse pesticides from 2001 to 2022 ----
###
rm(list=ls())
library(openxlsx)
library(ggplot2)
library(showtext)
showtext_auto(enable = TRUE)
font_add('Songti', 'Songti.ttc')

## load trained models
setwd("../data/")
load("fitted_model.RData")
reference_file = "4_pesticide_lists_for_temporal_trends_quantitation.xlsx"
Data_base2000 = read.xlsx(reference_file,sheet = 1)
Data_forbidden = read.xlsx(reference_file,sheet = 2)
Data_predict = read.xlsx(reference_file,sheet = 3)
Data_predict$register_year = sapply(Data_predict$`Pesticides`,function(x){
  tmp = Data_forbidden$`registration_years`[which(Data_forbidden$`Pesticides`==x)]
  return(unique(tmp))
})
Data_predict$forbidden_year = sapply(Data_predict$`Pesticides`,function(x){
  Data_forbidden$`forbidden_years`[which(Data_forbidden$`Pesticides`==x)]
})
## Standardized Labels
Data_base2000$Application_modes = gsub("Vehicle-mounted pump","VMP",Data_base2000$Application_modes)
Data_predict$Application_modes = gsub("Vehicle-mounted pump","VMP",Data_predict$Application_modes)

if(!dir.exists("./Scenario_Simulation/")){
  dir.create("Scenario_Simulation")
}
output_path = "./Scenario_Simulation/"
setwd(output_path)
## Select the prediction types: upper, predict, lower
type_value = "predict"
level = 0.9
## Set the start and end years for the prediction
start_year = 2001
end_year = 2022
tmp_predict = matrix(nrow = nrow(Data_predict),ncol = end_year - start_year + 1)
colnames(tmp_predict) = start_year:end_year
tmp_predict = as.data.frame(tmp_predict)

fit_forbidden = ret_list[["forbidden"]][["fitted_model"]]
max_y = round(max(ret_list[["forbidden"]][["CI_pred"]][["sam_y"]]),digits = 3)
for(list_name in names(ret_list)[-length(ret_list)]){
  tmp_list = ret_list[[list_name]]
  tmp_mode = unlist(strsplit(list_name,split = "-"))[1]
  tmp_type = unlist(strsplit(list_name,split = "-"))[2]
  
  prd_id = which(Data_predict$`Application_modes`==tmp_mode & Data_predict$`Types`==tmp_type)
  
  fit.sol = ret_list[[list_name]][["fitted_model"]]
  for(k in prd_id){
    time_st = start_year - Data_predict$register_year[k]
    time_ed = end_year - Data_predict$register_year[k]
    tx_tmp = time_st:time_ed
    
    resg_min = 0
    resg_max = Inf
    time_seq = 0
    if(!is.na(Data_predict$forbidden_year[k])){
      resg_max = Data_predict$forbidden_year[k] - Data_predict$register_year[k] + 1
      time_seq = end_year - Data_predict$forbidden_year[k]
    }
    CI_pred = CI_pred_1 = CI_pred_2 = ref_pred_1 = ref_pred_2 = data.frame()
    ## Temporal Trend Quantification after the registration year
    id_tmp = which(tx_tmp>resg_min & tx_tmp<=resg_max)
    CI_pred_1 = data.frame(x=tx_tmp[id_tmp])
    tmp_CI_pred = predict(fit.sol,newdata = CI_pred_1, interval = "confidence",level = level)
    if(type_value=="lower"){
      CI_pred_1$y = tmp_CI_pred[,"lwr"]
    }else if(type_value=="upper"){
      CI_pred_1$y = tmp_CI_pred[,"upr"]
    }else{
      CI_pred_1$y = tmp_CI_pred[,"fit"]
    }
    ref_pred_1 = tmp_CI_pred[,"fit"]
    ## Outlier handling
    if(length(which(CI_pred_1$y<0))>0){
      CI_pred_1$y[which(CI_pred_1$y<0)] = 0
    }

    if(time_seq > 0){
      ## Temporal Trend Quantification after the forbidden year
      CI_pred_2 = data.frame(x=1:time_seq)
      tmp_CI_pred = predict(fit_forbidden,newdata = CI_pred_2, interval = "confidence")
      if(type_value=="lower"){
        CI_pred_2$y = tmp_CI_pred[,"lwr"]
      }else if(type_value=="upper"){
        CI_pred_2$y = tmp_CI_pred[,"upr"]
      }else{
        CI_pred_2$y = tmp_CI_pred[,"fit"]
      }
      ref_pred_2 = tmp_CI_pred[,"fit"]
      
      CI_pred_2$y = sapply(CI_pred_2$y,function(y){
        # Predicted Value Transformation (S-shaped curve)
        max_y * exp(y) / (1+exp(y))
      })
      ref_pred_2 = max_y * exp(ref_pred_2) / (1+exp(ref_pred_2))

      if(Data_predict$`year`[k] > Data_predict$forbidden_year[k]){
        loca_id = Data_predict$`year`[k] - Data_predict$forbidden_year[k]
        CI_pred_2$y = Data_predict$`Application_rates_(g/ha/yr)`[k] * CI_pred_2$y / ref_pred_2[loca_id]
        ref_val = CI_pred_1$y[length(CI_pred_1$y)]
        if(ref_val == 0){
          break
          tmp_ref_val = Data_predict$`Application_rates_(g/ha/yr)`[k] * ref_pred_2 / ref_pred_2[loca_id]
          translate_val = tmp_ref_val[1] / ref_pred_1[length(ref_pred_1)]
        }else{
          translate_val = CI_pred_2$y[1] / ref_val
        }
        CI_pred_1$y = translate_val * CI_pred_1$y
      }else{
        known_year = which((start_year:end_year)[id_tmp]==Data_predict$`year`[k])
        CI_pred_1$y = Data_predict$`Application_rates_(g/ha/yr)`[k] * CI_pred_1$y / ref_pred_1[known_year]
        ref_val = CI_pred_2$y[1]
        translate_val = CI_pred_1$y[length(CI_pred_1$y)] / ref_val
        CI_pred_2$y = translate_val * CI_pred_2$y
      }
      CI_pred = rbind(CI_pred_1[-length(CI_pred_1$y),],CI_pred_2)
    }else{
      known_year = which((start_year:end_year)[id_tmp]==Data_predict$`year`[k])
      CI_pred_1$y = Data_predict$`Application_rates_(g/ha/yr)`[k] * CI_pred_1$y / ref_pred_1[known_year]
      CI_pred = CI_pred_1
    }
    
    id_pred = which(tx_tmp>0)
    tmp_predict[k,id_pred] = CI_pred$y
  }
}
ret_Data = cbind(Data_predict,tmp_predict)
## output predicted results
if(type_value == "predict"){
  write.xlsx(ret_Data,paste0("ret_Data_",type_value,".xlsx"))
}else{
  write.xlsx(ret_Data,paste0("ret_Data_",type_value,level*100,"%CI.xlsx"))
}

