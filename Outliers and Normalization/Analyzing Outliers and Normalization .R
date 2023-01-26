rm(list = ls(all = TRUE))

# Loading the dataset for the analysis
library(vcd)
data("iris") 

# Various ways of summarizing the data
summary(iris)

by(iris, iris$Species, summary)

library(Hmisc)
describe(iris)

library(pastecs)
stat.desc(iris)

library(ggpubr)
ggsummarystats(
  iris, x = "Species", y = "Sepal.Length", 
  ggfunc = ggboxplot, add = "jitter",
  color = "Species", palette = "npg"
)
ggsummarystats(
  iris, x = "Species", y = "Petal.Length", 
  ggfunc = ggviolin, add = "jitter",
  color = "Species", palette = "npg"
)

# Outlier check with IQR based approach for Sepal.Length
iqr<- IQR(iris$Sepal.Length) 
Q1<- quantile(iris$Sepal.Length, 0.25)
Q3<- quantile(iris$Sepal.Length, 0.75)
x.SL<- as.numeric(c(Q1-1.5*iqr, Q3+1.5*iqr))
x.SL
summary(iris$Sepal.Length)

# Outlier check with IQR based approach for Sepal.Width
iqr<- IQR(iris$Sepal.Width) 
Q1<- quantile(iris$Sepal.Width, 0.25)
Q3<- quantile(iris$Sepal.Width, 0.75)
x.SW<- as.numeric(c(Q1-1.5*iqr, Q3+1.5*iqr))
x.SW
summary(iris$Sepal.Width) 
#Setting outlier to NA
iris$Sepal.Width[iris$Sepal.Width < x.SW[1] | iris$Sepal.Width > x.SW[2]] = NA
iris$Sepal.Width

# Outlier check with IQR based approach for Petal.Length
iqr<- IQR(iris$Petal.Length) 
Q1<- quantile(iris$Petal.Length, 0.25)
Q3<- quantile(iris$Petal.Length, 0.75)
x.PL<- as.numeric(c(Q1-1.5*iqr, Q3+1.5*iqr))
x.PL
summary(iris$Petal.Length)

# Outlier check with IQR based approach for Petal.Width
iqr<- IQR(iris$Petal.Width) 
Q1<- quantile(iris$Petal.Width, 0.25)
Q3<- quantile(iris$Petal.Width, 0.75)
x.PW<- as.numeric(c(Q1-1.5*iqr, Q3+1.5*iqr))
x.PW
summary(iris$Petal.Width)

# Z-score based Normalization on Sepal.Length
mean_Sepal.Length<- mean(iris$Sepal.Length, na.rm=TRUE) 
sd_Sepal.Length<- sqrt(var(iris$Sepal.Length, na.rm=TRUE))
iris$Sepal.Length_std<- (iris$Sepal.Length-mean_Sepal.Length)/sd_Sepal.Length
summary(iris$Sepal.Length)
summary(iris$Sepal.Length_std)

# Z-score based Normalization on Sepal.Width
mean_Sepal.Width<- mean(iris$Sepal.Width, na.rm=TRUE) 
sd_Sepal.Width<- sqrt(var(iris$Sepal.Width, na.rm=TRUE))
iris$Sepal.Width_std<- (iris$Sepal.Width-mean_Sepal.Width)/sd_Sepal.Width
summary(iris$Sepal.Width)
summary(iris$Sepal.Width_std)

# Z-score based Normalization Petal.Length
mean_Petal.Length<- mean(iris$Petal.Length, na.rm=TRUE) 
sd_Petal.Length<- sqrt(var(iris$Petal.Length, na.rm=TRUE))
iris$Petal.Length_std<- (iris$Petal.Length-mean_Petal.Length)/sd_Petal.Length
summary(iris$Petal.Length)
summary(iris$Petal.Length_std)

# Z-score based Normalization Petal.Width
mean_Petal.Width<- mean(iris$Petal.Width, na.rm=TRUE) 
sd_Petal.Width<- sqrt(var(iris$Petal.Width, na.rm=TRUE))
iris$Petal.Width_std<- (iris$Petal.Width-mean_Petal.Width)/sd_Petal.Width
summary(iris$Petal.Width)
summary(iris$Petal.Width_std)

#Shapiro-Wilko normality hypothesis test on Sepal.Length
shapiro.test(iris$Sepal.Length) #Comment:probability less than 5%, so normality hypothesis is not TRUE.

#Shapiro-Wilko normality hypothesis test on Sepal.Width
shapiro.test(iris$Sepal.Width) #Comment:probability greater than 5%, so normality hypothesis is TRUE.

#Shapiro-Wilko normality hypothesis test on Petal.Length
shapiro.test(iris$Petal.Length) #Comment:probability less than 5%, so normality hypothesis is not TRUE.

#Shapiro-Wilko normality hypothesis test on Petal.Width
shapiro.test(iris$Petal.Width) #Comment:probability less than 5%, so normality hypothesis is not TRUE.

#Box-Cox transformation on Sepal.Length
library(MASS)
transf <- boxcox(iris$Sepal.Length ~ iris$Petal.Length)
lambda <- transf$x[which.max(transf$y)]
Sepal.Length_bct <- (iris$Sepal.Length^lambda-1)/lambda
shapiro.test(Sepal.Length_bct)

# Plotting the results side by side
par(mfrow = c(1, 2))

#Q-Q plot for original model
qqnorm(iris$Sepal.Length,main = 'Original Model')
qqline(iris$Sepal.Length)

#Q-Q plot for Box-Cox transformed model
qqnorm(Sepal.Length_bct,main = 'Box-Cox Transformed Model')
qqline(Sepal.Length_bct)

#rmarkdown::render("Saad.R", "pdf_document")
