rm(list=ls())
library(openxlsx)
library(Cairo)

#####Model adjusting
data_adj = read.xlsx("data_adjust.xlsx",sheet = 1)
fun_xm = function(x){exp(-0.437*x)+0.013}
fun_ym = function(x){exp(-0.2644*x)+0.0035}
fun_sd = function(x){exp(-0.372*x)+0.003}

fun_c = function(c,k,s){
  val = k*T_sum - data_adj[s,3]
  pro_new = c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))+n_list*c*pro_stapplesInProv[,s]
  val = val-sum(K_vec[pro_stapplesInProv[,s]!=0]*pro_new[pro_stapplesInProv[,s]!=0])
  return(val)
}

n_xm = 30; n_ym = 41; n_sd = 59
n_list = c(n_xm,n_ym,n_sd)
pro_notStaple = round(data_adj[,2]/sum(data_adj[,2]),digits = 4)

pro_stapplesInProv = apply(data_adj[,4:6],1,FUN = function(x){
  temp_X = x*c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))
  round(temp_X/sum(temp_X),digits = 4)})
colnames(pro_stapplesInProv) = data_adj[,1]
T_sum = sum(data_adj[,3])

ret_paraC = data.frame(province=character(),dA_ori=numeric(),dA=numeric(),c_min=numeric(),c_max=numeric(),c_optim=numeric(),a=numeric(),pro_dA=numeric())
for(s in 1:length(pro_notStaple)){
  k = pro_notStaple[s]
  K_vec = sapply(4:6,function(x){k*sum(data_adj[-s,x])+(k-1)*data_adj[s,x]})
  
  A_staple_ori = sum(data_adj[s,4:6]*sapply(pro_stapplesInProv[,s],function(x){ifelse(x==0,0,1)})*
                       c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd))))
  A_notStaple_ori = k*(T_sum-sum(sapply(4:6,function(x){sum(data_adj[,x])})*sapply(pro_stapplesInProv[,s],function(x){ifelse(x==0,0,1)})*
                                   c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))))
  B_s = data_adj[s,3]
  dA_ori = A_staple_ori+A_notStaple_ori-B_s
  
  set_step = 0.01
  c_min = -c(fun_xm(n_xm),fun_ym(n_ym),fun_sd(n_sd))*sapply(pro_stapplesInProv[,s],function(x){ifelse(x==0,0,1/x)})
  c_min = max(c_min[which(pro_stapplesInProv[,s]!=0)])
  c_max = (1-c(fun_xm(1),fun_ym(1),fun_sd(1)))*sapply(pro_stapplesInProv[,s],function(x){ifelse(x==0,0,1/x)})
  c_max = min(c_max[which(pro_stapplesInProv[,s]!=0)])
  tx = seq(c_min,c_max,by=set_step)
  if(!any(tx==0)){
    tx = append(tx,0)
  }
  if(!any(tx==c_max)){
    tx = append(tx,c_max)
  }
  ty=sapply(tx,FUN = function(x){fun_c(c=x,k=k,s=s)})
  data_temp = data.frame(tx,ty)
  c_optim = data_temp$tx[which.min(abs(data_temp$ty))]
  pro_new_sum = c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))+n_list*c_optim*pro_stapplesInProv[,s]
  
  A_staple = sum(data_adj[s,4:6]*pro_new_sum)
  A_notStaple = k*(T_sum-sum(sapply(4:6,function(x){sum(data_adj[,x])})*pro_new_sum))
  dA = A_staple+A_notStaple-B_s
  pro_dA = dA/B_s
  ret_paraC = rbind(ret_paraC,data.frame(province=data_adj[s,1],dA_ori,dA,c_min,c_max,c_optim,pro_dA))
}
detail_paras = ret_paraC[order(abs(ret_paraC$pro_dA),decreasing = T),]
write.csv(ret_paraC,"detail_paras.csv",row.names = F)


sum_temp = apply(data_adj[,4:6],2,sum)*c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))
k_total = round(sum_temp/sum(sum_temp),digits = 4)
fun_error = function(x,k_s,A_s,B_s){
  A_staple_all = sum(apply(data_adj[,4:6],2,sum)*(c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))+n_list*k_total*x))
  A_notStaple_s = (T_sum-A_staple_all)*k_s
  A_staple_s = sum(A_s*(c(sum(fun_xm(1:n_xm)),sum(fun_ym(1:n_ym)),sum(fun_sd(1:n_sd)))+n_list*k_total*x))
  return(abs(A_staple_s+A_notStaple_s-B_s))
}
c_list = unique(ret_paraC$`c_optim`)
c_list = seq(0.145,0.215,by=0.001)
total_error = numeric(length = length(c_list))
for(i in 1:length(c_list)){
  c_test = c_list[i]
  total_error[i] = sum(sapply(1:length(pro_notStaple),FUN = function(x){fun_error(c_test,pro_notStaple[x],data_adj[x,4:6],data_adj[x,3])}))
}
para_optimC = c_list[which.min(total_error)]
para_optimC
error_list = data.frame(c_list,total_error)
error_list = error_list[order(error_list$total_error),]
write.csv(error_list,"error_list.csv",row.names = F)
para_optimC*k_total

par(mai=c(0.9,0.9,0.3,0.3),las=3)
plot(error_list,type="l",xlab = "parameter C",ylab = "Abs Total Error")
