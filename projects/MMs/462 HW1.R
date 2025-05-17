setwd("/Users/itsxuuu/Desktop/462 Marketing Models/Data")
ad=read.csv("pre-roll_ad.csv")

#check dataset information
str(ad)
summary(ad)
library(dplyr)
glimpse(ad)

# device type (PC Web, Mobile Web, Mobile App)
# ad_complete
# linear probability regression
m1 <- lm(ad_complete ~ media_platform
   + genre_new
   + ad_brand_cat
   + age_new
   + day
   + tt
   + clip_duration,
   ad)
summary(m1)

#Logit (Logistic) Regression
m2 <- glm(ad_complete ~ media_platform
                + genre_new
                + ad_brand_cat
                + age_new
                + day
                + tt
                + clip_duration,
                family = binomial(link = "logit"),
                ad)
summary(m2)

1 / (1 + exp(-0.3773240))
1 / (1 + exp(-0.8670380))


#Probit Regression 
m3 <- glm(ad_complete ~ media_platform
          + genre_new
          + ad_brand_cat
          + age_new
          + day
          + tt
          + clip_duration,
          family = binomial(link = "probit"),
          ad)
summary(m3)

probit_value = 0.1927932  # Example for MOBILE WEB
pnorm(0.1927932)
pnorm(0.4850537)

#time interval
install.packages("survival")
library(survival)
str(ad)
ad$type = relevel(as.factor(ad$media_platform), ref="MOBILE APP")
ad$cens = 3

m4 <- survreg(Surv(l_ad_stop, u_ad_stop, cens, type = "interval") ~ 
            media_platform
          + genre_new
          + ad_brand_cat
          + age_new
          + day
          + tt
          + clip_duration,
          ad, dist = "gaussian")
summary(m4)


#age and gender
# linear
str(ad)
ad$age_new = relevel(as.factor(ad$age_new), ref="10")
m5 <- lm(ad_complete ~ age_new
         + gender_new
         + media_platform 
         + genre_new 
         + ad_brand_cat 
         + day 
         + tt, 
         ad)
summary(m5)

m5i <- lm(ad_complete ~ age_new * gender_new
         + media_platform 
         + genre_new 
         + ad_brand_cat 
         + day 
         + tt, 
         ad)
summary(m5i)
