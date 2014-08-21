#
# Leah Comment
#
# August 21, 2014
#
# Methods summer prep class session 5
#
#

# setwd("C:/Users/leahcomment/Desktop/vc_practice")
# getwd()

#now I've made a change!

#Libraries
library(lattice)
library(RColorBrewer)

#General settings
blues = brewer.pal(n=9, name="PuBu")
med.blue = blues[5]










# Modeling gamma-generated data as exponential ----------------------------
#yet another change

#Requested function to generate samples from the gamma distribution
PlotGamma <- function(n, alpha, beta, R) {
  #Generate the samples
  my.samples = matrix(data=rgamma(n=n*R, shape=alpha, rate=beta)
                      ,nrow=n
                      ,ncol=R)
  #Calculate sample stats
  sample.rate.ests = 1/colMeans(my.samples)
  overall.rate.est = 1/mean(my.samples)
  print("overall.rate.est")
  print(overall.rate.est)
  
  #Histogram of rate parameter estimates
  print(histogram(sample.rate.ests
                  ,main=paste("Estimated exponential parameters for",R,"replicate samples of size",n)
                  ,sub=paste0("True gamma distribution alpha=",alpha, ", beta=",beta)
                  ,col=med.blue
                  ,xlim=c(0
                         ,max(1.1*sample.rate.ests, 1.5*overall.rate.est))
                  ,freq=FALSE
                  ,xlab="Estimated rate parameter"))

}


#Run the function with different inputs
PlotGamma(n=1000, alpha=2, beta=2, R=10000)
PlotGamma(n=1000, alpha=1, beta=2, R=10000)






# Law of large numbers example --------------------------------------------

#Function to generate R size n samples from binomial, exponential, or chisq
#Saves mean of each replicate and plots histogram of sample means
ShowLLN <- function(distribution, R, n) {
  #Sample
  if (toupper(distribution) == "BINOMIAL") {
    #Generate the samples
    my.samples = matrix(data=rbinom(n=n*R, size=6, prob=0.3)
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[3]
  } else if (toupper(distribution) == "EXPONENTIAL") {
    #Generate the samples
    my.samples = matrix(data=rexp(n=n*R)
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[4]
  } else if (toupper(distribution) == "CHISQ") {
    #Generate the samples
    my.samples = matrix(data=rchisq(n=n*R,df=1)
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[5]
  } else {
    warning(paste("FUNCTION showlln CANNOT UNDERSTAND DISTRIBUTION INPUT",distribution))
  }
  
  #Save the means
  sample.means = colMeans(my.samples)
  
  #Histogram of means
  hist(sample.means
      ,main=paste0(toupper(distribution))
      ,sub=paste0("R=",R," n=",n)
      ,col=hist.color
      ,freq=FALSE
      ,xlab="Sample mean"
      ,xlim=c(0,3) ) 
  
}




#Use the function
sample.sizes = c(10, 100, 1000)
distributions = c("binomial", "exponential", "chisq")

#Plot for the different sample sizes
#Loop over distributions
par(mfrow=c(3,3))
for (distribution in distributions) {
  #Loop over sample sizes
  for (n in sample.sizes) {    
    #Plot for each of the sample sizes
    ShowLLN(distribution=distribution,R=100,n=n)
  }
}




# Central Limit Theorem example -------------------------------------------

#Function to generate R size n samples from binomial, exponential, or chisq
#Saves mean of each replicate and plots histogram of sample means
#Standardizes and plots to show the CLT
ShowCLT <- function(distribution, R, n) {
  #Means for the different distributions
  dist.means = c(0.3, 1, 1)
  dist.vars = c(dist.means[1]^2, dist.means[2]^-1, 2*dist.means[3])
  
  #Sample
  if (toupper(distribution) == "BINOMIAL") {
    #Generate the samples
    my.samples = matrix(data=rbinom(n=n*R, size=6, prob=dist.means[1])
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[3]
    #Set mean
    true.mean = dist.means[1]*6
    #Set variance
    true.variance = dist.vars[1]
    
  } else if (toupper(distribution) == "EXPONENTIAL") {
    #Generate the samples
    my.samples = matrix(data=rexp(n=n*R, rate=1/dist.means[2])
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[4]
    #Set mean
    true.mean = dist.means[2]
    #Set variance
    true.variance = dist.vars[2]
    
  } else if (toupper(distribution) == "CHISQ") {
    #Generate the samples
    my.samples = matrix(data=rchisq(n=n*R,df=dist.means[3])
                        ,nrow=n
                        ,ncol=R)
    #Set histogram color
    hist.color = brewer.pal(n=5,name="Set3")[5]
    #Set mean
    true.mean = dist.means[3]
    #Set variance
    true.variance = dist.vars[3]
    
  } else {
    warning(paste("FUNCTION showlln CANNOT UNDERSTAND DISTRIBUTION INPUT",distribution))
  }
  
  #Save the means
  sample.means = colMeans(my.samples)
  sample.means.standardized = (sample.means - true.mean)*sqrt(n)
  
  #Histogram of means
  hist(sample.means.standardized
      ,main=paste0(toupper(distribution))
      ,sub=paste0("R=",R,", n=",n)
      ,col=hist.color
      ,freq=FALSE
      ,xlab="Sample 'standardized' mean"
      ,ylim=c(0,0.5)
      ,xlim=c(-5,5) 
      )
  #Add normal overlay
  lines(x=seq(-5,5,0.1),y=dnorm(seq(-5,5,0.1)))
  
}




#Use the function
sample.sizes = c(10, 100, 1000)
distributions = c("binomial", "exponential", "chisq")

#Plot for the different sample sizes
#Loop over distributions
par(mfrow=c(length(sample.sizes),length(distributions)))
for (distribution in distributions) {
  #Loop over sample sizes
  for (n in sample.sizes) {    
    #Plot for each of the sample sizes
    ShowCLT(distribution=distribution,R=10000,n=n)
  }
}



