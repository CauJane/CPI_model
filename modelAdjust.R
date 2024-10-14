rm(list=ls())
library(openxlsx)
library(Cairo)

#####Model adjusting
data_adj = read.xlsx("data_adjust.xlsx",sheet = 1)
fun_xm = function(x){exp(-0.437*x)+0.013}
fun_ym = function(x){exp(-0.2644*x)+0.0035}
fun_sd = function(x){exp(-0.372*x)+0.003}

## non-staple cultivars
fun_veg = function(x){exp(-0.106*x)+0.024}
fun_fru = function(x){exp(-0.361*x)+0.007}
fun_tea = function(x){exp(-0.382*x)-0.009}
fun_cot = function(x){exp(-0.183*x)+0.035}
fun_rape = function(x){exp(-0.407*x)-0.062}
fun_gnut = function(x){exp(-0.64*x)+0.09}
fun_soyb = function(x){exp(-0.255*x)+0.023}
fun_lent = function(x){exp(-0.15*x)-0.002}
fun_pota = function(x){exp(-0.349*x)+0.004}

fun_c = function(Bs = NA,AR_list = numeric(),n_list=numeric(),f_list=numeric()){
  pred_Bs_list = AR_list*c(sum(fun_xm(1:n_list[1])),sum(fun_sd(1:n_list[2])),sum(fun_ym(1:n_list[3])),
                           sum(fun_veg(1:n_list[4])),sum(fun_fru(1:n_list[5])),sum(fun_tea(1:n_list[6])),
                           sum(fun_cot(1:n_list[7])),sum(fun_rape(1:n_list[8])),sum(fun_gnut(1:n_list[9])),
                           sum(fun_soyb(1:n_list[10])),sum(fun_lent(1:n_list[11])),sum(fun_pota(1:n_list[12])))
  optim_c = (Bs - sum(pred_Bs_list))/sum(AR_list*n_list*f_list)
  return(optim_c)
}


n_list = c(30,59,41,52,48,10,23,7,25,11,21,17)
A_staple_ori = apply(data_adj[,4:15],1,FUN = function(x){
  temp_X = x*c(sum(fun_xm(1:n_list[1])),sum(fun_sd(1:n_list[2])),sum(fun_ym(1:n_list[3])),
               sum(fun_veg(1:n_list[4])),sum(fun_fru(1:n_list[5])),sum(fun_tea(1:n_list[6])),
               sum(fun_cot(1:n_list[7])),sum(fun_rape(1:n_list[8])),sum(fun_gnut(1:n_list[9])),
               sum(fun_soyb(1:n_list[10])),sum(fun_lent(1:n_list[11])),sum(fun_pota(1:n_list[12])))
  return(temp_X)
  })
colnames(A_staple_ori) = data_adj[,1]

ret_paraC = data.frame(province=character(),dA_ori=numeric(),dA=numeric(),c_optim=numeric(),pro_dA=numeric())
for(s in 1:nrow(data_adj)){
  B_s = data_adj[s,3]
  AR_list = data_adj[s,4:15]
  f_list = data_adj[s,16:27]/sum(data_adj[s,16:27])
  dA_ori = sum(A_staple_ori[,s])-B_s
  
  c_optim = fun_c(Bs = B_s,AR_list = AR_list,n_list = n_list,f_list = f_list)
  dA = sum(A_staple_ori[,s])+sum(n_list*f_list*AR_list*c_optim)-B_s
  pro_dA = dA/B_s
  ret_paraC = rbind(ret_paraC,data.frame(province=data_adj[s,1],dA_ori,dA,c_optim,pro_dA))
}
write.csv(ret_paraC,"adjust_paras.csv",row.names = F)
