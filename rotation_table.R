# This file contains codes for making rotation tables for DSI studies
# Created by Betty Wu (Jiaman) in September 2019
# Last updated by Betty in September 2019



# install.packages("agricolae")
library(agricolae)

# get_designs( ) creats latin squares based tables with n replications
get_designs = function(t, n){
  
  d = data.frame()
  
  for (i in seq(1:n)) {
    
    trt = sample(LETTERS[1:t], t)
    outdesign = design.lsd(trt, serie=2)
    d = rbind(d, outdesign$sketch)
    
  }
  return(d)
}

# check_variance( ) returns the variance for a given table design
check_variance = function(d){
  
  combo.d = list()
  for(i in seq(1:(ncol(d) - 1))){
    combo.d = unlist(append(combo.d, paste(d[,i], d[,i+1])))
  }
  
  var(table(combo.d))
}

# perfect.design( ) makes R numbers of tables randomly and return the design with the lowest variance.
# A histogram of the variances is also available.
# See the message in console for the variance.

perfect.design = function(t, n, R){
  
  many.designs = lapply(1:R, function(i){
    d = get_designs(t,n)
  })
  
  many.variances = sapply(many.designs, check_variance)
  
  the.design = many.designs[which.min(many.variances)][[1]]
  
  message("The variance is ",  many.variances[which.min(many.variances)])
  hist(many.variances, main = "Histogram of variances for all designs")
  return(the.design)
  
}


# opt.design( ) return the optimal(lowest variance) table based on R different potential table if the number of
# participants in the study is not a multiple of the number of samples.

opt.design = function(per.design, t,n,p, R){
  
  many.designs = lapply(1:R, function(i){
    d = per.design[sample(seq(1:(t*n)), p),]
  })
  
  many.variances = sapply(many.designs, check_variance)
  
  the.design = many.designs[which.min(many.variances)][[1]]
  
  message("The variance for the optimized design is ",  many.variances[which.min(many.variances)])
  hist(many.variances, main = "Histogram of Variances for the final designs")
  return(the.design)
  
}


rotation.table = function(t, p, R = 1000){
  
  n = ceiling(p/t) # number of replications
  
  per.design = perfect.design(t, n, R)
  
  if (nrow(per.design) == p) {
    
    return(per.design)
    
    
  }else{
    
    opt.table = opt.design(per.design,t, n, p, R)
    # message("Number of rows is ", nrow(opt.table))
    return(opt.table)
    
  }
  
}

# RUN THIS!!!

# t is the number of treatments
# p is the number of participants in the study
# R is how many searches we want R to do (this is an optional argument, the default is 1000)


rotation.table(t = 7, p = 80) # example
