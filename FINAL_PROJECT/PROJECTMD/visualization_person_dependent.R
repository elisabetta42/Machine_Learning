source('/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/multiplot.R')
library('scales')

var_k <-c(1,3,5,7,11,21,31,41,51,61,71,81,91,101)
cluster<-c(10,20,50,70,80,100,200,300,400)
result
pd_accuracy<-vector('list')
pd_time<-vector('list')
temporary_acc<-vector('list')
temporary_time<-vector('list')
cluster_plot_list<-vector()
cluster_plot_time<-vector()
#k_pd_cluster_matrix<-matrix(ncol =length(pd_accuracy[[1]]) ,nrow=length(s_accuracy)) 
#time_pd_cluster_matrix<-matrix(ncol =length(pd_accuracy[[1]]) ,nrow=length(s_accuracy)) 
#Plot clusters with times

#calculate mean of all the runs for every k inside every cluster
for(c in 1:length(result)){
  temporary_acc[[c]]<-apply(result[[c]]$v1,2,mean)
  temporary_time[[c]]<-apply(result[[c]]$v2,2,mean)
}
#select the highest k for plotting
for(j in 1:length(temporary_acc)){
  cluster_plot_list[j]<-(max(temporary_acc[[j]]))
  cluster_plot_time[j]<-(max(temporary_time[[j]]))
}

#Plot clusters accuracy
interval_y<-createsequence(0, 1,0.02)
interval_x<-createsequence(0, 400,20)
jpeg(file="/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/plots/cluster_pd.png",width = 700,height=500)

x<-cluster
pd_y<- cluster_plot_list

##Style of errorbars:

pd_cluster<-qplot(x,pd_y) +
  geom_line() +
  geom_point()+
  scale_x_continuous(breaks = interval_x) +
  scale_y_continuous(breaks = interval_y)

pd_cluster<-pd_cluster+labs(title="Clustering Data (person dependent learning problem)", x="Increasing number of clusters", y = "Accuracy in percentage")+
  theme_classic() 

#Plot clusters time

x<-cluster
t_pd_y<- cluster_plot_time

##Style of errorbars:

t_pd_cluster<-qplot(x,t_pd_y) +
  geom_line() +
  geom_point()
  

t_pd_cluster<-t_pd_cluster+labs(title="Time in seconds for every cluster (person dependent learning problem)", x="Increasing number of clusters", y = "Accuracy in percentage")+
  theme_classic() 

multiplot(pd_cluster,t_pd_cluster)
dev.off()

#plot k for person dependent - Accuracy
jpeg(file="/Users/elisabetta/Desktop/ML/Machine_Learning/FINAL_PROJECT/PROJECTMD/plots/knn_pd.png",width = 700,height=500)

x<-var_k
m_y <-dp_k_list$v1
y <- apply(m_y,2,mean)
m_sd <- apply(m_y,2,sd)
m_sd=m_sd*10
dp_k<-qplot(x,y) +
  geom_line() +
  scale_x_continuous(breaks = x) +
  scale_y_continuous(breaks = interval_y) +
  geom_errorbar(aes(ymin=y-m_sd, ymax=y+m_sd), width=.0,
                position=position_dodge(0.00))

dp_k<-dp_k+labs(title="Accuracy for best cluster using multiple k values (person dependent learning problem)", x="Increasing number of k", y = "Accuracy in percentage")+
  theme_classic() 

#plot k for person dependent - Time
x<-var_k
tt_y<-dp_k_list$v2
t_y <- apply(tt_y,2,mean)
t_sd <- apply(tt_y,2,sd)

t_dp_k<-qplot(x,t_y) +
  geom_line() +
  scale_x_continuous(breaks = x) +
  geom_errorbar(aes(ymin=t_y-t_sd, ymax=t_y+t_sd), width=.0,
                position=position_dodge(0.00))

t_dp_k<-t_dp_k+labs(title="Time for best cluster using multiple k values (person dependent learning problem)", x="Increasing number of k", y = "Time in seconds")+
  theme_classic() 

multiplot(dp_k,t_dp_k) 
dev.off()
