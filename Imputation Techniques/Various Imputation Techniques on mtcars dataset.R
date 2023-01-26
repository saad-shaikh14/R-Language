rm(list = ls(all = TRUE))
library(impute)
library(Amelia)

data("mtcars")
View(mtcars)
mtcars1 <- mtcars

# Variable with missing values; setting level of missingness of 15%
mtcars1_imp <- mtcars1; 
mis_level <- 0.15 

# Setting 15% of "mpg" values as NA
set.seed(1234)
a = (mtcars1$mpg)
a2 = sample(length(a), round(length(a)*mis_level),replace = FALSE)
a2

#Setting mpg to NA
mtcars1[a2, "mpg"] <- NA
mtcars1$mpg[a2]
missmap(mtcars1)



#mean imputation
mtcars1_mean <- mtcars1
mtcars1_mean$mpg[is.na(mtcars1$mpg)] <- mean(mtcars1$mpg, na.rm = TRUE)
mtcars1_mean$mpg

#Root mean square error for mean imputation
mtcars1[a2,1]
mtcars1_mean[a2,1]
m_r_m <- sqrt(sum((mtcars[a2,1]-mtcars1_mean[a2,1])^2) / length(a2)) #rmse
m_r_m

# k-nearest neighbour (with k=5)
library(VIM)
Mcars <- kNN(mtcars1, variable = c("mpg"), k = 5)
summary(Mcars)

#Root mean square error for KNN imputation
mtcars1[a2,1]
Mcars[a2,1]
k_r_m <- sqrt(sum((mtcars[a2,1]-Mcars[a2,1])^2) /length(a2))
k_r_m

# Amelia imputation
mtcars_a <- amelia(mtcars1, m=5)
mtcars1_a_imp = (mtcars_a$imputations$imp1+mtcars_a$imputations$imp2
                 +mtcars_a$imputations$imp3+mtcars_a$imputations$
                   imp4+mtcars_a$imputations$imp5)/5
mtcars1_a_imp

#Root mean square error for Amelia imputation
a_rms = sqrt(sum((mtcars[a2,1]-mtcars1_a_imp[a2,1])^2) /length(a2))
a_rms


#Implementing MAR Mechanism.
#setting 30% values to NA for mpg using wt

mtcars2 <- mtcars
summary(mtcars$mpg)
a11 = which (mtcars1$mpg > 15 & mtcars1$mpg < 25)
a11
set.seed(1234)
a21 = sample (a11,round(length(a11)*0.3))
a21

mtcars2[a21, "wt"] <- NA
mtcars2$wt[a21]
missmap(mtcars2)

#mean imputation on mar
mtcars2_mean <- mtcars2 
mtcars2_mean$wt[is.na(mtcars2_mean$wt)] <- mean(mtcars2$wt, na.rm = TRUE)
mtcars2_mean$wt

#Root mean square error for mean imputation
mtcars2[a21,6]
mtcars2_mean[a21,6]
mean_r_m <-sqrt(sum((mtcars[a21,6]-mtcars2_mean[a21,6])^2) /length(a21))
mean_r_m

# k-nn

library(VIM)
mtcars2_k <- kNN(mtcars2, variable = c("wt"), k = 5)
mtcars2_k$wt
mtcars2_mean$wt

#Root mean square error for KNN imputation

mtcars2[a21,6]
mtcars2_k[a21,6]
knn_r_m <- sqrt(sum((mtcars[a21,6]-mtcars2_k[a21,6])^2) /length(a21))
knn_r_m

# Amelia imputation
mtcars2_a <- amelia(mtcars2, m=5)
mtcars2_a_imp = (mtcars2_a$imputations$imp1+mtcars2_a$imputations$imp2+mtcars2_a$imputations$imp3+mtcars2_a$imputations$imp4+mtcars2_a$imputations$imp5)/5
mtcars2_a_imp

#Root mean square error for Amelia imputation
a_rms1 = sqrt(sum((mtcars[a21,6]-mtcars2_a_imp[a21,6])^2) /length(a21))
a_rms1

