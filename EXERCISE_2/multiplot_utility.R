# plot all the accuracies for person dependent
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

##################################################################################################
#plot accuracies for person dependent
##################################################################################################

df1 <- data.frame(
  person_dependent = factor(c("80% cumulative variance","80% cumulative variance","80% cumulative variance","80% cumulative variance"
                              ,"80% cumulative variance","80% cumulative variance","80% cumulative variance",
                              "80% cumulative variance","90% cumulative variance",
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance",
                              "95% cumulative variance",
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance",
                              "99% cumulative variance",
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance")),
  k = factor(c(1, 5, 11, 15, 21, 25, 31, 35,1, 5, 11, 15, 21, 25, 31, 35), levels=c(1, 5, 11, 15, 21, 25, 31, 35)),
  accuracies =c(0.5339063,0.5475, 0.5433438,0.5434687,0.5406875,0.537875,
                                 0.5339063,0.53225,0.5825625,0.5879688,0.58325,0.5809688,
                                 0.576125,0.5712812,0.5695938,0.5654062,0.5929688,0.59425,
                                 0.591125,0.584375,0.5811875,0.5774687,0.5729688,0.5700312,
                                 0.5938438,0.59625,0.5896875,0.5851563,0.5798125,0.5755,0.57,
                                 0.5678437)
)

# A basic graph

per_dep_acc <- ggplot(data=df1, aes(x=k, y=accuracies, group=person_dependent,colour=person_dependent)) + geom_line() 



##################################################################################################
#plot times for person dependent
##################################################################################################

df1 <- data.frame(
  person_dependent = factor(c("80% cumulative variance","80% cumulative variance","80% cumulative variance","80% cumulative variance"
                              ,"80% cumulative variance","80% cumulative variance","80% cumulative variance",
                              "80% cumulative variance","90% cumulative variance",
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance",
                              "95% cumulative variance",
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance",
                              "99% cumulative variance",
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance")),
  k = factor(c(1, 5, 11, 15, 21, 25, 31, 35,1, 5, 11, 15, 21, 25, 31, 35), levels=c(1, 5, 11, 15, 21, 25, 31, 35)),
  times_minutes = c(57.75185,58.70407,58.48638,1.011228,59.62939,58.43497,57.66686,57.79611,
                               2.069033,2.055091,2.085673,2.08979,2.151394,2.317071,2.251707,2.206996,
                               4.094808,4.196481,4.007309,4.106943,4.08494,4.021613,4.08363,4.183792,
                               7.369942,7.401155,7.36142,7.336522,7.454709,7.472859,7.49811,7.475994)
)

# A basic graph

per_dep_time <- ggplot(data=df1, aes(x=k, y=times_minutes, group=person_dependent,colour=person_dependent)) + geom_line() 

multiplot(per_dep_acc,per_dep_time)



##################################################################################################
#plot accuracies for person independent
##################################################################################################

df1 <- data.frame(
  person_independent = factor(c("80% cumulative variance","80% cumulative variance","80% cumulative variance","80% cumulative variance"
                              ,"80% cumulative variance","80% cumulative variance","80% cumulative variance",
                              "80% cumulative variance","90% cumulative variance",
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance",
                              "95% cumulative variance",
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance",
                              "99% cumulative variance",
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance")),
  k = factor(c(1, 5, 11, 15, 21, 25, 31, 35,1, 5, 11, 15, 21, 25, 31, 35), levels=c(1, 5, 11, 15, 21, 25, 31, 35)),
  accuracies =c(0.8125938,0.8150312,0.808,0.7996562,0.7885937,
  0.7814688,0.7732813,0.7696563,0.8539688,0.8496563,0.8343438,
  0.8273125,0.8143125,0.8080625,0.8004062,0.7954375,0.86425,
  0.8529062,0.8374375,0.82975,0.8174687,0.8118125,0.8027188,
  0.7966875,0.8649375, 0.8538125,0.837,0.8283125,0.8145,0.80775,
  0.7985313, 0.7931563)
)

# A basic graph

per_indep_acc <- ggplot(data=df1, aes(x=k, y=accuracies, group=person_independent,colour=person_independent)) + geom_line() 



##################################################################################################
#plot times for person dependent
##################################################################################################

df1 <- data.frame(
  person_independent = factor(c("80% cumulative variance","80% cumulative variance","80% cumulative variance","80% cumulative variance"
                              ,"80% cumulative variance","80% cumulative variance","80% cumulative variance",
                              "80% cumulative variance","90% cumulative variance",
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance", "90% cumulative variance", "90% cumulative variance", 
                              "90% cumulative variance",
                              "95% cumulative variance",
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance", "95% cumulative variance", "95% cumulative variance", 
                              "95% cumulative variance",
                              "99% cumulative variance",
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance", "99% cumulative variance", "99% cumulative variance", 
                              "99% cumulative variance")),
  k = factor(c(1, 5, 11, 15, 21, 25, 31, 35,1, 5, 11, 15, 21, 25, 31, 35), levels=c(1, 5, 11, 15, 21, 25, 31, 35)),
  times_minutes = c(55.63824,55.27387,55.66097,56.66266,57.4731,56.15492,58.68535,57.09879,
  2.038594,2.080253,2.093811,2.051259,2.018174,2.074277,2.137008,2.116798,
  4.087,4.100913,4.064693,4.048095,4.018301,4.084872,4.115202,4.091371,
  7.30866,7.316201,7.315804,7.485649,7.520651,7.378566,7.496262,7.467105)
)

# A basic graph

per_indep_time <- ggplot(data=df1, aes(x=k, y=times_minutes, group=person_independent,colour=person_independent)) + geom_line() 

multiplot(per_indep_acc,per_indep_time)





