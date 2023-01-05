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
######## ks_stat_ucb1: calculate ks statistic 
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
## return: test statistic of the ks test and Dnm at the specific tau value
######
## the output plot shows user cdf and system cdf .
## test-statistic value is shown below title and other parameters are shown at the bottom-right.
######
##########
ks_stat_ucb1 <- function(K=10,T=1000,min_mean=0,max_mean=10,min_sd=0,max_sd=1,
                     rec_func.=rec_func,alpha=0,tau=0.5,plot=TRUE){
  means <- runif(K, min=min_mean, max=max_mean)
  sds <- runif(K, min=min_sd, max=max_sd)
  real_matrix <<- matrix(nrow=K, ncol=T)
  for (i in 1:K){
    real_matrix[i,] <<- rnorm(T, means[i], sds[i])
  }
  rec <<- rec_func(real_matrix,alpha)
  
  ## user side sample
  user_sample_vector <<- rep(0,tau*T)
  
  ## to store experimental means of each K arm
  exp_means <<- rep(0,K)
  ## first k turns: initialize experimental means by pulling each arm once
  for (j in 1:K){
    exp_means[j] <<- real_matrix[j,j]
    ## record the sample
    user_sample_vector[j] <<- real_matrix[j,j]
  }
  
  ## to store ucb for each K arm
  ucb <- rep(0,K)
  ## to store number of times each K arm pulled. intialize with each pulled once
  times_pulled <- rep(1,K)
  for (i in (K+1):(tau*T)){
    for (j in 1:K){
      ucb[j] <- sqrt(2*log(i)/times_pulled[j])
    }
    ## this is the arm to pull with largest sum
    arm_to_pull <- which.max(exp_means+ucb)
    ## reward associate with the arm
    temp_reward <- real_matrix[arm_to_pull,i]
    ## record in user sample side
    user_sample_vector[i] <<- temp_reward
    ## recalculate the experimental mean of that arm pulled
    exp_means[arm_to_pull] <<- 
      (exp_means[arm_to_pull]*times_pulled[arm_to_pull]+temp_reward)/(times_pulled[arm_to_pull]+1)
    ## increase times the arm pulled by 1
    times_pulled[arm_to_pull] <- times_pulled[arm_to_pull] + 1
  }
  
  cdf_user <- ecdf(user_sample_vector)
  
  ## system side
  system_sample_vector <- rec[(tau*T+1):T]
  cdf_system <- ecdf(system_sample_vector)
  
  observed <- c(user_sample_vector,system_sample_vector)
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
  
  reject_level <- 1.358*sqrt(T/(tau*T*(T-tau*T)))
  
  if (plot==TRUE){
    plot(observed,user_prob_vector,type='l',xlim=c(x_lower,x_upper),col='black',ylab='CDF prob',
         main=paste('KS Test under tau =',tau))
    lines(observed,system_prob_vector,type='l',xlim=c(x_lower,x_upper),col='red',ylab='CDF prob')
    legend("bottomright", legend=c('user','system'), col=c('black','red'),lty=1:1,cex=0.5*par('cex'))
    points(c(x, x), c(user_y, system_y), pch=16, col="blue",cex=0.8*par('cex')) 
    segments(x, user_y, x, system_y, col="blue", lty="dotted")
    mtext(paste(paste('K =',K),paste('T =',T),paste('alpha =',alpha),sep='\n'), 
          side=1, line=3.5, at=x_upper,cex=0.7*par('cex'))
    mtext(paste(paste('test-statistic =',round(diff,5),'  '),
                paste('rejection level =', round(reject_level,5))),cex=0.7*par('cex'))
  }
  return(c(diff,reject_level))
}

set.seed(199)
ks_stat_ucb1()

## create trend plot for tau from 0.01 to 0.99
set.seed(199)
tau_vector <- seq(0.01,0.99,0.01)
ts_vector <- rep(NA,length(tau_vector))
reject_vector <- rep(NA,length(tau_vector))

for (i in 1:length(tau_vector)){
  ts_vector[i] <- ks_stat_ucb1(tau=tau_vector[i],T=10000,plot=FALSE)[1]
  reject_vector[i] <- ks_stat_ucb1(tau=tau_vector[i],T=10000,plot=FALSE)[2]
}

## display the plot
plot(beta_vector, ts_vector,type='l',xlab='tau',ylim=c(0,1.2),col='red',ylab='TS',main="test-statistic against tau")
lines(beta_vector,reject_vector,type='l')
points(beta_vector[which.min(ts_vector)],ts_vector[which.min(ts_vector)],pch=16, col="black")
text(beta_vector[which.min(ts_vector)],ts_vector[which.min(ts_vector)],
     labels=beta_vector[which.min(ts_vector)],pos=4,cex=0.7)
legend("topright",legend=c('TS','reject'), col=c('red','black'),lty=1:1,cex=0.5)

## create trend plot for alpha from 
set.seed(199)
alpha_vector <- seq(0,8,0.05)
ts_alpha_vector <- rep(NA,length(alpha_vector))
reject_alpha_vector <- rep(ks_stat_ucb1(T=10000,plot=FALSE)[2],length(alpha_vector))
for (i in 1:length(alpha_vector)){
  ts_alpha_vector[i] <- ks_stat_ucb1(T=10000,alpha=alpha_vector[i],plot=FALSE)[1]
}

plot(alpha_vector, ts_alpha_vector,type='l',xlab='alpha',col='red',
     ylab='TS',main="test-statistic against alpha")
lines(alpha_vector,reject_alpha_vector,type='l')
mtext("red line: Dnm \n black line: Rejection",side=1,
      at=alpha_vector[length(alpha_vector)],line=3.5,cex=0.7)







