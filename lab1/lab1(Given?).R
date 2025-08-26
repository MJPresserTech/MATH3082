#' ---
#' title: "Lab 1"
#' author: "Mohammad Shihab Uddin"
#' date: "`r Sys.Date()`"
#' output:
#'   pdf_document:
#'     toc: true
#'     number_sections: true
#' 
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' \newpage
#' 
#' # Basic Command
#' 
## ---- warning=FALSE------------------------------------------------------------------
6+3
6*3
6-3
6/9
sqrt(25)
log(10)

#' 
#' 
#' # Creating Vector
#' 
## ---- warning=FALSE------------------------------------------------------------------
eyec1 = c("Green", "Brown", "Hazel", "Brown", "Gray")
GPA1 = c(2.5, 3, 3.8, 2.7, 3.2, 3.5)

#' 
#' 
#' # Vector Subscripting
#' 
## ---- warning=FALSE------------------------------------------------------------------
x<-c(34,56,76,43)
x[c(4,1,4)]

#' 
#' # Vector Operation
#' 
## ---- warning=FALSE------------------------------------------------------------------
x <- c(10,5,3,6)
z <- c(7,8,9,2)
y1 = x + 3
y2 = x + z
x*z
z/x
log(x)
log(z)

#' 
#' 
#' # creating Matrix
#' 
## ---- warning=FALSE------------------------------------------------------------------
mat <- matrix(data = GPA1, nrow = 2, ncol = 3, byrow = TRUE)
mat

#' 
#' # creating data frame
#' 
## ---- warning=FALSE------------------------------------------------------------------
Number<-c(1,2,3,4)
Diet<-c("Poor","Poor","Good","Good")
Sex<-c("M","F","M","F")
Weight<-c(156,180,167,190)
Fat.content<-c(34,43,40,43)
Morph<-c("Winged","Winged","Wingless","Intermediate")

cricket_dat<-data.frame(Number, Diet, Sex,
                            Weight, Fat.content, Morph)
cricket_dat

names(cricket_dat)<-c("No","Diet","Gender",
                     "WT","FC", "Morph")
head(cricket_dat)


#' 
#' 
#' 
#' # Extracting Vectors
#' 
## ---- warning=FALSE------------------------------------------------------------------
GPA1 = c(2.5, 3, 3.8, 2.7, 3.2) 
eyec1 = c("Green", "Brown", "Hazel", "Brown", "Gray")
interest1 = c(2, 5, 4, 4, 3)
mydata1 = data.frame(GPA = GPA1, eye.color = eyec1, interest = interest1)

mydata1$eye.color
mydata1$GPA1 < 3 


#' 
#' # Data Manipulation
#' 
#' 
#' ## Filtering
#' 
## ---- warning=FALSE------------------------------------------------------------------
good_diet_data <- cricket_dat[cricket_dat$Diet == "Good", ]
print("Data for Good Diet:")
print(good_diet_data)

#' 
#' 
#' ## Select rows where Weight is greater than 170
#' 
## ---- warning=FALSE------------------------------------------------------------------
heavy_weight_data <- cricket_dat[cricket_dat$WT > 170, ]
print("Data for Weight > 170:")
print(heavy_weight_data)

#' 
#' 
#' ## Subsetting
## ---- warning=FALSE------------------------------------------------------------------
## Select specific columns (e.g., No and Morph)
subset_data <- cricket_dat[, c("No", "Morph")]
print("Subset of Data with No and Morph:")
print(subset_data)

#' 
#' ## Creating New Variables
#' 
## ---- warning=FALSE------------------------------------------------------------------
## Calculate BMI (Body Mass Index)
cricket_dat$BMI <- cricket_dat$WT / ((cricket_dat$FC / 100)^2)
print("Data with BMI:")
print(cricket_dat)

#' 
#' 
#' ## saving data frame
#' 
## ---- warning=FALSE------------------------------------------------------------------
write.csv(cricket_dat, "cricket.csv")

#' 
#' 
#' # Apply Function
#' 
## ---- warning=FALSE------------------------------------------------------------------
my_matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
my_matrix

column_means <- apply(my_matrix, 2, mean)
column_means

row_means <- apply(my_matrix, 1, mean)
row_means


#' 
#' # Tapply Function
#' 
## ---- warning=FALSE------------------------------------------------------------------
ID <- 1:10
marks <- c(22,23,20,17,15,11,20,15,17,19)
sex<-c("f","m","m","f","m","f","f","m","m","m")
df <- data.frame(ID, marks, sex)
tapply(df$marks, df$sex, mean)


#' 
#' 
#' #  Descreptive Statistics
#' 
## ---- warning=FALSE------------------------------------------------------------------
my_data <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60)
mean_value <- mean(my_data)
median_value <- median(my_data)
variance_value <- var(my_data)
sd_value <- sd(my_data)

#' 
#' # Graphics
#' 
#' ## Scatter
#' 
## ---- warning=FALSE------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 5, 4, 6)
plot(x, y, main="Scatter Plot", xlab="X-axis", ylab="Y-axis", col="blue", pch=16)

#' 
#' ## Line Plot
#' 
## ---- warning=FALSE------------------------------------------------------------------
x <- seq(0, 2*pi, length.out=100)
y <- sin(x)
plot(x, y, type="l", main="Line Plot", xlab="X-axis", ylab="Y-axis", col="red", lwd=2)

#' 
#' 
#' ## Histogram
#' 
## ---- warning=FALSE------------------------------------------------------------------
data <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 5)
hist(data, main="Histogram", xlab="Values", col="green", border="black")

#' 
#' ## Boxplot
#' 
## ---- warning=FALSE------------------------------------------------------------------
group1 <- c(1, 2, 2, 3, 3)
group2 <- c(4, 4, 4, 5, 5)
boxplot(group1, group2, names=c("Group 1", "Group 2"), col=c("blue", "orange"), main="Boxplot")

#' 
#' ## Multiple Plots
#' 
## ---- warning=FALSE------------------------------------------------------------------
par(mfrow=c(2, 2))  # 2x2 layout
plot(x, sin(x), main="Plot 1", type="l", col="blue")
hist(c(1, 2, 2, 3, 3), main="Plot 2", xlab="Values", col="green", border="black")
boxplot(c(1, 2, 2, 3, 3), main="Plot 3", col="purple")
barplot(table(c("A", "B", "C")), main="Plot 4", col="orange")

