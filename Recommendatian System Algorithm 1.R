library(pheatmap)
## Set number of Arms
K = 10
## Set number of trials
T = 1000
## Set seed to get the same result
set.seed(199)
## Array store arm normal distribution mean
means <- runif(K, min=0, max=10)
## Array store arm normal distribution sd
sds <- runif(K, min=0, max=1)
## matrix to store KT sample 
real_matrix <- matrix(nrow=K, ncol=T)
## generate sample KT size 
for (i in 1:K){
  real_matrix[i,] <- rnorm(T, means[i], sds[i])
} 

## recommendation sheet 1
## Here we assume that the system can always access the real matrix and report max reward
rec1 <- rep(NA, T)
for (i in 1:T){
  rec1[i] <- max(real_matrix[,i])
}
## recommendation sheet 2
## Here we assume that the system gives random recommendation
rec2 <- rep(NA, T)
for (i in 1:T){
  k <- sample(1:K,1,replace=TRUE)
  rec2[i] <- real_matrix[k,i]
}

## recommendation sheet 3
## The system gives optimal solution at alpha% chance, otherwise random instead
alpha = 95
rec3 <- rep(NA, T)
for (i in 1:T){
  ifelse(runif(1,0,100)<alpha, rec3[i]<-max(real_matrix[,i]), 
         rec3[i]<-real_matrix[sample(1:K,1,replace=TRUE),i])
}

## recommendation sheet 4
## The system picks the theoretical max value from largest mean in guassion
rec4 <- rep(NA, T)
for (i in 1:T){
  rec4[i] <- real_matrix[which.max(means),i]
}

## recommendation sheet 5 
## The system gives optimal solution at alpha% chance, otherwise largest mean in guassion
rec5 <- rep(NA, T)
for (i in 1:T){
  ifelse(runif(1,0,100)<alpha, rec5[i]<-max(real_matrix[,i]),
               rec5[i]<-real_matrix[which.max(means),i])
}

###########################
##single trial#############-----------------
###########################

## Samples Users have before tau times
set.seed(199)
tau <- 0.1*T
sample_data <- rep(NA, tau)
times_arm <- tau/K
for (i in 1:K){
  for (j in (1+(i-1)*times_arm):(times_arm*i)){
    sample_data[j] <- real_matrix[i,j]
  }
}
## set alpha value for extracting training data from samples 
beta <- 0.2
train_data <- sort(sample_data,decreasing = TRUE)[1:(beta*tau)]
## Test data provided by the recommendation system
test_data <- rec1[(tau+1):T]
## run the test
ks.test(train_data, test_data)$p.value

############################-------------------

## try different tau values and store in the table
set.seed(199)
times_tau <- 9
times_beta <- 9
## using recommendation sheet 1
result1 <- matrix(NA,nrow=times_tau,ncol=times_beta)
## try tau from 0.1 to 0.9 and beta from 0.1 to 0.9
colnames(result1) <- paste("alpha",seq(0.1,0.9,0.1),sep='')
rownames(result1) <- paste("tau",seq(0.1,0.9,0.1),sep='')
for (i in 1:times_tau){
  tau <- i*0.1*T
  ## Samples Users have
  sample_data <- rep(NA, tau)
  ## calculate the times that users draw each arm
  times_arm <- tau/K
  ## test data provided by the recommendation system
  test_data <- rec1[(tau+1):T]
  ## draw sample data tau/K times for each K arms
  for (j in 1:K){
    for (q in (1+(j-1)*times_arm):(times_arm*j)){
      sample_data[q] <- real_matrix[j,q]
    }
  }
  for (m in 1:times_beta){
  ## Samples extracted to form train data
  train_data <- sort(sample_data,decreasing=TRUE)[1:(m*0.1*tau)]
  result1[i,m] <- ks.test(train_data,test_data)$p.value
  }
}

## using recommendation sheet 2
result2 <- matrix(NA,nrow=times_tau,ncol=times_beta)
## try tau from 0.1 to 0.9 and beta from 0.1 to 0.9
colnames(result2) <- paste("alpha",seq(0.1,0.9,0.1),sep='')
rownames(result2) <- paste("tau",seq(0.1,0.9,0.1),sep='')
for (i in 1:times_tau){
  tau <- i*0.1*T
  ## Samples Users have
  sample_data <- rep(NA, tau)
  ## calculate the times that users draw each arm
  times_arm <- tau/K
  ## test data provided by the recommendation system
  test_data <- rec2[(tau+1):T]
  ## draw sample data tau/K times for each K arms
  for (j in 1:K){
    for (q in (1+(j-1)*times_arm):(times_arm*j)){
      sample_data[q] <- real_matrix[j,q]
    }
  }
  for (m in 1:times_beta){
    ## Samples extracted to form train data
    train_data <- sort(sample_data,decreasing=TRUE)[1:(m*0.1*tau)]
    result2[i,m] <- ks.test(train_data,test_data)$p.value
  }
}

## using recommendation sheet 3
result3 <- matrix(NA,nrow=times_tau,ncol=times_beta)
## try tau from 0.1 to 0.9 and beta from 0.1 to 0.9
colnames(result3) <- paste("alpha",seq(0.1,0.9,0.1),sep='')
rownames(result3) <- paste("tau",seq(0.1,0.9,0.1),sep='')
for (i in 1:times_tau){
  tau <- i*0.1*T
  ## Samples Users have
  sample_data <- rep(NA, tau)
  ## calculate the times that users draw each arm
  times_arm <- tau/K
  ## test data provided by the recommendation system
  test_data <- rec3[(tau+1):T]
  ## draw sample data tau/K times for each K arms
  for (j in 1:K){
    for (q in (1+(j-1)*times_arm):(times_arm*j)){
      sample_data[q] <- real_matrix[j,q]
    }
  }
  for (m in 1:times_beta){
    ## Samples extracted to form train data
    train_data <- sort(sample_data,decreasing=TRUE)[1:(m*0.1*tau)]
    result3[i,m] <- ks.test(train_data,test_data)$p.value
  }
}

## using recommendation sheet 4
result4 <- matrix(NA,nrow=times_tau,ncol=times_beta)
## try tau from 0.1 to 0.9 and beta from 0.1 to 0.9
colnames(result4) <- paste("alpha",seq(0.1,0.9,0.1),sep='')
rownames(result4) <- paste("tau",seq(0.1,0.9,0.1),sep='')
for (i in 1:times_tau){
  tau <- i*0.1*T
  ## Samples Users have
  sample_data <- rep(NA, tau)
  ## calculate the times that users draw each arm
  times_arm <- tau/K
  ## test data provided by the recommendation system
  test_data <- rec4[(tau+1):T]
  ## draw sample data tau/K times for each K arms
  for (j in 1:K){
    for (q in (1+(j-1)*times_arm):(times_arm*j)){
      sample_data[q] <- real_matrix[j,q]
    }
  }
  for (m in 1:times_beta){
    ## Samples extracted to form train data
    train_data <- sort(sample_data,decreasing=TRUE)[1:(m*0.1*tau)]
    result4[i,m] <- ks.test(train_data,test_data)$p.value
  }
}

result1 <- round(result1,5)
result2 <- round(result2,5)
result3 <- round(result3,5)
result4 <- round(result4,5)
result5 <- round(result5,5)
## using recommendation sheet 5
result5 <- matrix(NA,nrow=times_tau,ncol=times_beta)
## try tau from 0.1 to 0.9 and beta from 0.1 to 0.9
colnames(result5) <- paste("alpha",seq(0.1,0.9,0.1),sep='')
rownames(result5) <- paste("tau",seq(0.1,0.9,0.1),sep='')
for (i in 1:times_tau){
  tau <- i*0.1*T
  ## Samples Users have
  sample_data <- rep(NA, tau)
  ## calculate the times that users draw each arm
  times_arm <- tau/K
  ## test data provided by the recommendation system
  test_data <- rec5[(tau+1):T]
  ## draw sample data tau/K times for each K arms
  for (j in 1:K){
    for (q in (1+(j-1)*times_arm):(times_arm*j)){
      sample_data[q] <- real_matrix[j,q]
    }
  }
  for (m in 1:times_beta){
    ## Samples extracted to form train data
    train_data <- sort(sample_data,decreasing=TRUE)[1:(m*0.1*tau)]
    result5[i,m] <- ks.test(train_data,test_data)$p.value
  }
}

### line plots

matplot(result1,type='l',xlab='alpha value',ylab="p-value",main='recommendation 1')
abline(h=0.05,col='red',lty='dotted')
legend("right",legend=rownames(result1),col=c(1:9), lty=1:9, inset=0.01)

matplot(result2,type='l',xlab='alpha value',ylab="p-value",main='recommendation 2')
abline(h=0.05,col='red',lty='dotted')
legend("left",legend=rownames(result1),col=c(1:9), lty=1:9, inset=0.01)

matplot(result3,type='l',xlab='alpha value',ylab="p-value",main='recommendation 3')
abline(h=0.05,col='red',lty='dotted')
legend("right",legend=rownames(result1),col=c(1:9), lty=1:9, inset=0.01)

matplot(result4,type='l',xlab='alpha value',ylab="p-value",main='recommendation 4')
abline(h=0.05,col='red',lty='dotted')
legend("top",legend=rownames(result1),col=c(1:9), lty=1:9, inset=0.01)

matplot(result5,type='l',xlab='alpha value',ylab="p-value",main='recommendation 5')
abline(h=0.05,col='red',lty='dotted')
legend("right",legend=rownames(result1),col=c(1:9), lty=1:9, inset=0.01)


### heat maps

pheatmap(result1,cluster_rows = F, cluster_cols = F, main = "recommendation sheet 1")

pheatmap(result2,cluster_rows = F, cluster_cols = F, main = "recommendation sheet 2")

pheatmap(result3,cluster_rows = F, cluster_cols = F, main = "recommendation sheet 3")

pheatmap(result4,cluster_rows = F, cluster_cols = F, main = "recommendation sheet 4")

pheatmap(result5,cluster_rows = F, cluster_cols = F, main = "recommendation sheet 5")




#####
library(reshape2)
ggplot(melt(result1), aes(x=Var1, y=Var2, fill=value))


#### overleaf file 

### p1 have acceess p2  only mean distribution p3 combine p1 p2 by some proportion. 

### tau value determination to find the best tau value 
### seed values    
### why increasing T gives unexpected results 

ks.test(rec1[1:(0.8*T)],rec1[((0.8*T)+1):T])


### question: ks test on discrete values 
## is it possible to convert arm k to a continuous value
### to many ties that make KS test work poorly

### how to optimize / 


### wednesday 5 pm 

