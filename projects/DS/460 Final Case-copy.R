
library(tidyverse)
library(car)
library(cluster)

setwd("/Users/itsxuuu/Desktop/460 Data Science Foundations/Data")
dat = read.csv("churnclass.csv") %>%
  mutate(sessions = mobile + tablet + desktop+app+other)
dat1<-dat%>%filter(sample==1)
dat2<-dat1%>%rowwise() %>%mutate(devicesum=sum(c_across(c(desktop,tablet,mobile,app,other))))%>%
  mutate(across(c(desktop, tablet, mobile, app, other), ~ if_else(devicesum > 0, . / devicesum, 0))) %>%
  select(-devicesum) %>%
  ungroup() 
datfinal<-dat2%>%select(desktop, tablet, mobile, app, other)
#clusterbasicfunction####
summary.kmeans = function(fit) 
{
  p = ncol(fit$centers)
  K = nrow(fit$centers)
  n = sum(fit$size)
  xbar = t(fit$centers)%*%fit$size/n
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)), fit$tot.withinss/(p*(n-K)))), 4)
  ))
  cat("SSE=", fit$tot.withinss, "; SSB=", fit$betweenss, "; SST=", fit$totss, "\n")
  cat("R-Squared = ", fit$betweenss/fit$totss, "\n")
  cat("Pseudo F = ", (fit$betweenss/(K-1))/(fit$tot.withinss/(n-K)), "\n\n");
  invisible(list(Rsqr=fit$betweenss/fit$totss, 
                 F=(fit$betweenss/(K-1))/(fit$tot.withinss/(n-K))))
}

plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}


set.seed(12345)
fit = kmeans(datfinal,4,100,100)
fit1 = kmeans(datfinal, 5, 100, 100)
fit2 = kmeans(datfinal, 6, 100, 100)
fit3 = kmeans(datfinal, 7, 100, 100)
fit4 = kmeans(datfinal, 8, 100, 100)
fit5 = kmeans(datfinal, 9, 100, 100)
plot(fit2)
F = double(6)
for(K in 4:9){
  set.seed(12345)
  F[K-3] = summary(
    kmeans(datfinal, K, nstart=100,100))$F
}

SSE = double(6)
for(K in 4:9){
  set.seed(12345)
  km_result = kmeans(datfinal, centers=K, nstart=100,100)
  SSE[K-3] = km_result$tot.withinss
}


plot(4:9, F, type="b", xlab="Number Clusters K")
plot(4:9, SSE, type="b", xlab="Number of Clusters (K)", ylab="SSE")



# Fit a logistic regression model predicting nextchurn from control variables log(sessions) and log(t), and your clusters (as a categorical/factor variable). What does it tell you?
fit2 = glm(nextchurn ~ log(t+1) + log(sessions+1) + clus, binomial, dat)
summary(fit2)

dat2$clus = factor(fit2$cluster, levels=1:6, labels=c("1", "2", "3","4", "5", "6"))
(tab=with(dat2, table(clus, nextchurn)))
prop.table(tab, 1)

dat2 %>%
  group_by(clus) %>%
  summarise(n=n(), nchurn=sum(nextchurn), churn=mean(nextchurn), 
            logsess=mean(log(sessions+1)), mut=mean(t), medsess=median(sessions))

logit = glm(nextchurn ~ log(t+1) + log(sessions+1) + clus, binomial, dat2)
summary(logit)







# Aris
set.seed(12345)
fit = kmeans(datfinal,4,100,100)
fit1 = kmeans(datfinal, 5, 100, 100)
fit2 = kmeans(datfinal, 6, 100, 100)
fit3 = kmeans(datfinal, 7, 100, 100)
fit4 = kmeans(datfinal, 8, 100, 100)
fit5 = kmeans(datfinal, 9, 100, 100)
plot(fit2)
summary(fit2)
#############
fit2 = kmeans(datfinal, 6, 100, 100)
dat2$clus = factor(fit2$cluster, levels=1:6, 
                   labels=c("The Old Schoolers", "Pocket News Junkies", "The Clingy","Polyamorous Lovers", "App Loyalists", "The Ghost-ers"))
(tab=with(dat2, table(clus, nextchurn)))
prop.table(tab, 1)
plot(fit2)

dat2 %>%
  group_by(clus) %>%
  summarise(n=n(), nchurn=sum(nextchurn), churn=mean(nextchurn), 
            logsess=mean(log(sessions+1)), mut=mean(t), medsess=median(sessions))

logit = glm(nextchurn ~ log(t+1) + log(sessions+1) + clus, binomial, dat2)
summary(logit)


dat2$clus <- relevel(as.factor(dat2$clus), ref ="Pocket News Junkies")
logit2 = glm(nextchurn ~ log(t+1) + log(sessions+1) + clus, binomial, dat2)
summary(logit2)












