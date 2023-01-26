# Assignment: 3                                
# Title: Predictive Modelling                
# NAME: SAAD SHAIKH                   
# ID: 20070328                               

# TASK 1
# First, we should choose a dataset for the assignment that contains at least one column
# with only 2 different values (should be binary). If the values are {0 or 1} we do not change
# them, or else we should recode it so that the values should be equal to {0, 1}. Write a brief summary of the
# dataset: enlisting the number of observations and features, and explaining the meaning of
# data and the role the individual features is showing. Choosing the binary column with {0, 1}
# values to be the outcome you want to predict.

#clear all lists
rm(list = ls(all = TRUE))

#loading data set
library(mlbench)
data(PimaIndiansDiabetes2)
a <- PimaIndiansDiabetes2

#summarizing the data set
summary(a) 

#handling NA values in columns by mean imputation
a$glucose[is.na(a$glucose)] <- mean(a$glucose, na.rm = TRUE)
a$pressure[is.na(a$pressure)] <- mean(a$pressure, na.rm = TRUE)
a$triceps[is.na(a$triceps)] <- mean(a$triceps, na.rm = TRUE)
a$insulin[is.na(a$insulin)] <- mean(a$insulin, na.rm = TRUE)
a$mass[is.na(a$mass)] <- mean(a$mass, na.rm = TRUE)
summary(a)

#Modifying the data to convert categorical outcome to binary
levels(a$diabetes) <- c(0,1) # binary outcome
a$diabetes <- as.numeric(a$diabetes)
a$diabetes <- a$diabetes - 1 
summary(a)

# TASK 2
# Normalizing the entire predictors using the Z-score transformation and checking the normality
# of their distributions using the Shapiro-Wilk test.

#Z-score transformation on pregnant column
p_mean <- mean(a$pregnant, na.rm = TRUE)
p_sd <- sqrt(var(a$pregnant, na.rm = TRUE))
a$p_normal <- (a$pregnant-p_mean)/p_sd
summary(a$p_normal)

#Z-score transformation on glucose column
g_mean <- mean(a$glucose, na.rm = TRUE)
g_sd <- sqrt(var(a$glucose, na.rm = TRUE))
a$g_normal <- (a$glucose-g_mean)/g_sd
summary(a$g_normal)

#Z-score transformation on pressure column
pr_mean <- mean(a$pressure, na.rm = TRUE)
pr_sd <- sqrt(var(a$pressure, na.rm = TRUE))
a$pr_normal <- (a$pressure-pr_mean)/pr_sd
summary(a$pr_normal)

#Z-score transformation on triceps column
tri_mean <- mean(a$triceps, na.rm = TRUE)
tri_sd <- sqrt(var(a$triceps, na.rm = TRUE))
a$tri_normal <- (a$triceps-tri_mean)/tri_sd
summary(a$tri_normal)

#Z-score transformation on insulin column
insulin_mean <- mean(a$insulin, na.rm = TRUE)
insulin_sd <- sqrt(var(a$insulin, na.rm = TRUE))
a$insulin_normal <- (a$insulin-insulin_mean)/insulin_mean
summary(a$insulin_normal)

#Z-score transformation on mass column
mass_mean <- mean(a$mass, na.rm = TRUE)
mass_sd <- sqrt(var(a$mass, na.rm = TRUE))
a$mass_normal <- (a$mass-mass_mean)/mass_sd
summary(a$mass_normal)

#Z-score transformation on pedigree column
pedigree_mean <- mean(a$pedigree, na.rm = TRUE)
pedigree_sd <- sqrt(var(a$pedigree, na.rm = TRUE))
a$pedigree_normal <- (a$pedigree-pedigree_mean)/pedigree_sd
summary(a$pedigree_normal)

#Z-score transformation on age column
mean_age <- mean(a$age, na.rm = TRUE)
sd_age <- sqrt(var(a$age, na.rm = TRUE))
a$age_normal <- (a$age-mean_age)/sd_age
summary(a$age_normal)

#Shapiro Test on pregnant column
shapiro.test(a$pregnant)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on glucose column
shapiro.test(a$glucose)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on pressure column
shapiro.test(a$pressure)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on triceps column
shapiro.test(a$tri_normal)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on insulin column
shapiro.test(a$insulin_normal)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on mass column
shapiro.test(a$mass_normal)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on pedigree column
shapiro.test(a$pedigree_normal)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

#Shapiro Test on age column
shapiro.test(a$age_normal)#comment: pvalue <0.05, hence a strong reason to reject normal hypothesis.

# TASK 3
# Split your Full data into a Training (70% of the data) and a Test (30% of the data) subsets.

# selecting only normalized columns
a <- a[c(9:17)]
str(a)
set.seed(123)
sample <- sample(seq_len(nrow(a)), size = 0.7*nrow(a))
a_training <- a[sample,]
a_test <- a[-sample,]

# TASK 4
# In the Training set we perform feature selection using the T, F and Wilcoxon scores.
# Then create a new subsets, the Reduced Training and the Reduced Test subsets, of
# the Training and Test sets, respectively, that contain only those features that ended
# up in Top 3 for at least one of the feature selection approaches used.

#Defining Function for feature selection: FSCR
FSCR = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X1<- X[which(Y==0),i]
    X2<- X[which(Y==1),i]
    mu1<- mean(X1); mu2<- mean(X2); mu<- mean(X[,i])
    var1<- var(X1); var2<- var(X2)
    n1<- length(X1); n2<- length(X2)
    J[i]<- (n1*(mu1-mu)^2+n2*(mu2-mu)^2)/(n1*var1+n2*var2)
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}
#Defining Function for feature selection: TSCR
TSCR = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X1<- X[which(Y==0),i]
    X2<- X[which(Y==1),i]
    mu1<- mean(X1); mu2<- mean(X2)
    var1<- var(X1); var2<- var(X2)
    n1<- length(X1); n2<- length(X2)
    J[i]<- (mu1-mu2)/sqrt(var1/n1+var2/n2)
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}
#Defining Function for feature selection: WLCX
WLCX = function(X, Y, k) # X - matrix with predictors, Y - binary outcome, k top candidates
{
  J<- rep(NA, ncol(X))
  names(J)<- colnames(X)
  for (i in 1:ncol(X))
  {
    X_rank<- apply(data.matrix(X[,i]), 2, function(c) rank(c))
    X1_rank<- X_rank[which(Y==0)]
    X2_rank<- X_rank[which(Y==1)]
    mu1<- mean(X1_rank); mu2<- mean(X2_rank); mu<- mean(X_rank)
    n1<- length(X1_rank); n2<- length(X2_rank); N<- length(X_rank)
    num<- (n1*(mu1-mu)^2+ n2*(mu2-mu)^2)
    denom<- 0
    for (j in 1:n1)
      denom<- denom+(X1_rank[j]-mu)^2
    for (j in 1:n2)
      denom<- denom+(X2_rank[j]-mu)^2
    J[i]<- (N-1)*num/denom
  }
  J<- sort(J, decreasing=TRUE)[1:k]
  return(list(score=J))
}

#Function variables
X <- a[c(2:9)] #predictors 
Y <- a[c(1)] #outcomes
k <- 3 #top-3 features

#Looking for top-3 features
FSCR(X, Y, k) #Top-3: g_normal, mass_normal, age_normal
WLCX(X, Y, k) #Top-3: g_normal, age_normal, mass_normal
TSCR(X, Y, k) #Top-3: pedigree_normal, pr_normal, insulin_normal

#defining reduced training and reduced test dataset
a_red_training <- a_training[c("g_normal", "mass_normal", "age_normal", "diabetes")]
a_red_test <- a_test[c("g_normal", "mass_normal", "age_normal", "diabetes")]

# TASK 5
# Train the logistic regression approach on both the Training set and the Reduced Training set.

# Logistic regression with training dataset
model1<- glm(diabetes~., data=a_training, family="binomial")
summary(model1)
#Logistic regression with reduced training dataset
model2<- glm(diabetes~., data=a_red_training, family="binomial")
summary(model2)

# TASK 6 
# Apply the trained models to both the Test set and the Reduced Test set. Then
# evaluate the Area under ROC-curve (AUC) for both cases and comment on whether
# feature reduction has heavily decrease the AUC for the Training set compared to the
# Reduced Training dataset.

#Discrimination
library(Hmisc); library(ggplot2); library(gridExtra)
# Predicting response: training model to test dataset
pr_model1<- predict(model1, a_test, type="response")
#Predicting response: reduced training model to reduced test dataset
pr_model2<- predict(model2, a_red_test, type="response")

# ROC-analysis
library(ROCR)
pred1.obj <- prediction(predictions = pr_model1, labels = a_test$diabetes)
pred2.obj  <- prediction(predictions = pr_model2,  labels = a_red_test$diabetes)

perf1 <- performance(pred1.obj, measure="tpr", x.measure="fpr")
perf2 <- performance(pred2.obj, measure="tpr", x.measure="fpr")

auc1<- somers2(pr_model1, a_test$diabetes)[1]; auc1
auc2<- somers2(pr_model2, a_red_test$diabetes)[1]; auc2

plot(perf1, lty = 1, col = "red", lwd = 3, cex.lab = 1.2)
plot(perf2,  lty = 1, col = "blue",  lwd = 3, add = T)
legend(0.45, 0.2, c("Model1", "Model2"), lty = c(1,1), col = c("red","blue"), lwd = c(3,3), cex = 0.8, bg = "gray90")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
abline(0, 1, col = "gray30", lwd = 1, lty = 2)

