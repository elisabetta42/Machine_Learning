require(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

list_times<-c(57.75185,58.70407,58.48638,1.011228,59.62939,58.43497,57.66686,57.79611,
        2.069033,2.055091,2.085673,2.08979,2.151394,2.317071,2.251707,2.206996,
        4.094808,4.196481,4.007309,4.106943,4.08494,4.021613,4.08363,4.183792,
        7.369942,7.401155,7.36142,7.336522,7.454709,7.472859,7.49811,7.475994,
        55.63824,55.27387,55.66097,56.66266,57.4731,56.15492,58.68535,57.09879,
        2.038594,2.080253,2.093811,2.051259,2.018174,2.074277,2.137008,2.116798,
        4.087,4.100913,4.064693,4.048095,4.018301,4.084872,4.115202,4.091371,
        7.30866,7.316201,7.315804,7.485649,7.520651,7.378566,7.496262,7.467105)

list_accuracies<-c(0.5339063,0.5475, 0.5433438,0.5434687,0.5406875,0.537875,
                   0.5339063,0.53225,0.5825625,0.5879688,0.58325,0.5809688,
                   0.576125,0.5712812,0.5695938,0.5654062,0.5929688,0.59425,
                   0.591125,0.584375,0.5811875,0.5774687,0.5729688,0.5700312,
                   0.5938438,0.59625,0.5896875,0.5851563,0.5798125,0.5755,0.57,
                   0.5678437,0.8125938,0.8150312,0.808,0.7996562,0.7885937,
                   0.7814688,0.7732813,0.7696563,0.8539688,0.8496563,0.8343438,
                   0.8273125,0.8143125,0.8080625,0.8004062,0.7954375,0.86425,
                   0.8529062,0.8374375,0.82975,0.8174687,0.8118125,0.8027188,
                   0.7966875,0.8649375, 0.8538125,0.837,0.8283125,0.8145,0.80775,
                   0.7985313, 0.7931563)
k<-c(1, 5, 11, 15, 21, 25, 31, 35)
a <- seq(1, length(list_accuracies), length(k))

##########################################################
#plot person dependent
##########################################################
# 80%
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_dep_80.jpg")
plot1 <- qplot(unlist(k), unlist(list_accuracies[1:8]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot2 <- qplot(unlist(k), unlist(list_times[1:8]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person dependent
##########################################################
# 90%
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_dep_90.jpg")
plot3 <- qplot(unlist(k), unlist(list_accuracies[9:16]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot4 <- qplot(unlist(k), unlist(list_times[9:16]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person dependent
##########################################################
# 95%
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_dep_95.jpg")
plot5 <- qplot(unlist(k), unlist(list_accuracies[17:24]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot6 <- qplot(unlist(k), unlist(list_times[17:24]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person dependent
##########################################################
# 99%
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_dep_99.jpg")
plot7 <- qplot(unlist(k), unlist(list_accuracies[25:32]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot8 <- qplot(unlist(k), unlist(list_times[25:32]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person independent
##########################################################
# 80
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_indep_80.jpg")
plot9 <- qplot(unlist(k), unlist(list_accuracies[33:40]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot10 <- qplot(unlist(k), unlist(list_times[33:40]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person independent
##########################################################
# 90
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_indep_90.jpg")
plot11 <- qplot(unlist(k), unlist(list_accuracies[41:48]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot12 <- qplot(unlist(k), unlist(list_times[41:48]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person independent
##########################################################
# 95
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_indep_95.jpg")
plot13 <- qplot(unlist(k), unlist(list_accuracies[49:56]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot14<- qplot(unlist(k), unlist(list_times[49:56]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()

##########################################################
#plot person independent
##########################################################
# 99
jpeg(file="Desktop/ML/EXERCISE_2/images/pers_indep_99.jpg")
plot15 <- qplot(unlist(k), unlist(list_accuracies[57:64]), geom = "line",xlab="k values",ylab="accuracies") + ggtitle("Accuracy for increasing k")
plot16 <- qplot(unlist(k), unlist(list_times[57:64]), geom = "line",xlab="k values",ylab="times (minutes)")+ ggtitle("Classification time for increasing k")
#multiplot(plot1, plot2)
dev.off()
multiplot(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8)
multiplot (plot9,plot10,plot11,plot12,plot13,plot14,plot15,plot16)