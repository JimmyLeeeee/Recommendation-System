##########
###### rec_func: the function that input real matrix and returns a recommendation system sheet
###### where the mode of the system is defined by alpha
## real_matrix: the real_matrix generated.
## alpha: for alpha% times the system does not offer the optimal choice. 
## if alpha = 0, the system has the access to the real matrix and always provide the optimal choice
## return: a recommendation system mode sheet as a vector
###########
rec_func <- function(real_matrix,alpha){
  rec <- rep(NA,dim(real_matrix)[2])
  for(i in 1:dim(real_matrix)[2]){
    ifelse(runif(1,0,100)<alpha, rec[i]<-real_matrix[sample(1:dim(real_matrix)[1],1,replace=TRUE),i],
           rec[i]<-max(real_matrix[,i]))
  }
  return(rec)
}


##########
######## ks_stat: calculate ks statistic 
## K: number of arms. Default K = 10
## T: total number of pull trials. Default T = 1000
## min_mean: minimum gaussian mean. Default min_mean = 0
## max_mean: maximum gaussian mean. Default max_mean = 10
## min_sd: minimum gaussian sd. Default min_sd = 0
## max_sd: maximum gaussian sd. Default max_sd = 1
## rec_func.: the function to generate system recommendation from real matrix. 
## rec_func.: Default rec_func = rec_func (predefined)
## alpha: for alpha% times the system does not offer the optimal choice. 
## alpha: Default alpha = 0, 0<=alpha<=100.
## tau: percentage of T that are randomly sampled by the users.Default = 0.5 (50%)
## beta: top beta percent largerst values of samples used for max cdf construction.
## beta: Default beta = 0.5,  0<=beta<=1.
## return: test statistic of the ks test and Dnm at the specific tau value
######
## the output plot shows user cdf and system cdf .
## test-statistic value is shown below title and other parameters are shown at the bottom-right.
######
##########
ks_stat2 <- function(K=10,T=1000,min_mean=0,max_mean=10,min_sd=0,max_sd=1,
                    rec_func.=rec_func,alpha=0,tau=0.5,beta=0.5,plot=TRUE){
  means <- runif(K, min=min_mean, max=max_mean)
  sds <- runif(K, min=min_sd, max=max_sd)
  real_matrix <<- matrix(nrow=K, ncol=T)
  for (i in 1:K){
    real_matrix[i,] <<- rnorm(T, means[i], sds[i])
  }
  rec <- rec_func(real_matrix,alpha)
  times <- tau*T/K
  user_sample_matrix <<- NULL
  for(j in 1:(tau*T)){
    user_sample_matrix <<- cbind(user_sample_matrix,
                                 real_matrix[sample(seq(K),1,replace=TRUE),j])
  }
  user_sample_vector <- as.vector(user_sample_matrix)
  sample_cdf_construction <- sort(user_sample_vector,decreasing = TRUE)[1:(beta*100)]
  cdf_user <- ecdf(sample_cdf_construction)
  system_sample_vector <- rec[(tau*T+1):T]
  cdf_system <- ecdf(system_sample_vector)
  
  observed <- c(sample_cdf_construction,system_sample_vector)
  user_prob_vector <- cdf_user(observed)
  system_prob_vector <- cdf_system(observed)
  diff <- max(abs(user_prob_vector-system_prob_vector))
  index <- which.max(abs(user_prob_vector-system_prob_vector))
  x <- observed[index]
  user_y <- user_prob_vector[index]
  system_y <- system_prob_vector[index]
  
  observed <- observed[order(observed)]
  user_prob_vector <- user_prob_vector[order(user_prob_vector)]
  system_prob_vector <- system_prob_vector[order(system_prob_vector)]
  
  user_non_zero <- which(user_prob_vector!=0)[1]
  system_non_zero <- which(system_prob_vector!=0)[1]
  x_lower <- observed[min(user_non_zero,system_non_zero)]
  x_upper <- observed[length(observed)]
  
  reject_level <- 1.358*sqrt((tau*T*beta+T-tau*T)/(tau*T*beta*(T-tau*T)))
  
  if (plot==TRUE){
    plot(observed,user_prob_vector,type='l',xlim=c(x_lower,x_upper),col='black',ylab='CDF prob',
         main=paste('KS Test under tau =',tau))
    lines(observed,system_prob_vector,type='l',xlim=c(x_lower,x_upper),col='red',ylab='CDF prob')
    legend("bottomright", legend=c('user','system'), col=c('black','red'),lty=1:1,cex=0.5*par('cex'))
    points(c(x, x), c(user_y, system_y), pch=16, col="blue",cex=0.8*par('cex')) 
    segments(x, user_y, x, system_y, col="blue", lty="dotted")
    mtext(paste(paste('K =',K),paste('T =',T),paste('alpha =',alpha),paste('beta =',beta),sep='\n'), 
          side=1, line=3.5, at=x_upper,cex=0.7*par('cex'))
    mtext(paste(paste('test-statistic =',round(diff,5),'  '),
                paste('rejection level =', round(reject_level,5))),cex=0.7*par('cex'))
  }
  return(c(diff,reject_level))
}




## one trial
set.seed(199)
ks_stat2(alpha=0,beta=0.01,T=10000)
ks_stat2(alpha=0,beta=0.5,T=10000)
ks_stat2(alpha=0,beta=1,T=10000)

## create trend plot for tau from 0.01 to 0.99
set.seed(199)
beta_vector <- seq(0.01,0.99,0.01)
ts_vector <- rep(NA,length(beta_vector))
reject_vector <- rep(NA,length(beta_vector))
for (i in 1:length(beta_vector)){
  ts_vector[i] <- ks_stat2(tau=0.5,beta=beta_vector[i],T=10000,plot=FALSE)[1]
  reject_vector[i] <- ks_stat2(tau=0.5,beta=beta_vector[i],T=10000,plot=FALSE)[2]
}

## display the plot
plot(beta_vector, ts_vector,type='l',xlab='beta',ylim=c(0,1.2),col='red',ylab='TS',main="test-statistic against beta")
lines(beta_vector,reject_vector,type='l')
points(beta_vector[which.min(ts_vector)],ts_vector[which.min(ts_vector)],pch=16, col="black")
# segments(tau_vector[which.min(ts_vector)],ts_vector[which.min(ts_vector)],
#          tau_vector[which.min(ts_vector)],0,col="blue", lty="dotted",lwd=2)
text(beta_vector[which.min(ts_vector)],ts_vector[which.min(ts_vector)],
     labels=beta_vector[which.min(ts_vector)],pos=4,cex=0.7)
legend("topright",legend=c('TS','reject'), col=c('red','black'),lty=1:1,cex=0.5)

