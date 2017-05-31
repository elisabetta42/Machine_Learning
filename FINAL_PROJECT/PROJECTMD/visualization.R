#source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/commander.R')
source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/multiplot.R')
library('scales')

var_k <-c(1,3,5,7,11,21,31,41,51,61,71,81,91,101)
# plot for single person
s_result #where results are store
s_accuracy<-vector('list')
s_time<-vector('list')
#separate time and accuracy
for(i in 1:length(s_result)){
  single_list<-s_result[i]
  s_accuracy[[i]]<-single_list[[1]]$v1
  s_time[[i]]<-single_list[[1]]$v2
}
sp_k_result<-vector('list')
sp_time_result<-vector('list')
list_k<-vector('list')
list_time<-vector('list')
#separate per k
for (i in 1:length(s_accuracy[[1]])){
  for(j in 1:length(s_accuracy)){
    list_k[[j]]<-s_accuracy[[j]][[i]]
    list_time[[j]]<-s_time[[j]][[i]]
  }
  sp_k_result[[i]]<-list_k #contains persons organized by k
  sp_time_result[[i]]<-list_time #contains persons organized by k
  
}

single_pers_accuracy<-matrix(ncol =length(s_accuracy[[1]]) ,nrow=length(s_accuracy)) 
single_pers_temp<-matrix(ncol =length(s_accuracy[[1]]) ,nrow=length(s_accuracy)) 

for (i in 1:length(s_accuracy)){
  for(j in 1:length(s_accuracy[[1]])){
    
    single_pers_accuracy[i,j]<-sp_k_result[[j]][[i]]
    single_pers_temp[i,j]<-sp_time_result[[j]][[i]]
    
  }
}
# Plot stuff
jpeg(file="/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/plots/single_person_acc_time.png")

#accuracy plot
x<-var_k
y<- colMeans(single_pers_accuracy, na.rm = TRUE)
sd<-apply(single_pers_accuracy,2,sd)
##Style of errorbars:

p<-qplot(x,y) +
  geom_line() +
  geom_point()+
  scale_x_continuous(breaks = x) +
  geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.02,
                position=position_dodge(0.02))

p<-p+labs(title="Accuracy (single person learning problem)", x="Increasing number of k", y = "Accuracy in percentage")+
  theme_classic() +
  scale_color_manual(values=c('#999999'))

#time plot

x<-var_k
t_y<- colMeans(single_pers_temp, na.rm = TRUE)
t_sd<-apply(single_pers_temp,2,sd)
##Style of errorbars:

time<-qplot(x,t_y) +
  geom_line() +
  geom_point()+
  scale_x_continuous(breaks = x) +
  geom_errorbar(aes(ymin=t_y-t_sd, ymax=t_y+t_sd), width=.02,
                position=position_dodge(0.02))

time<-time+labs(title="Time (single person learning problem)", x="Increasing number of k", y = "Running time in seconds")+
  theme_classic() +
  scale_color_manual(values=c('#999999'))

multiplot(p,time)
dev.off()

