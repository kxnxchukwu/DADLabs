##################################################################
##################################################################
##       Into to R Lab 2: Interacting with data frames          ##
##                                                              ##
##################################################################
##################################################################

x <- c(1:3, 7, 8:10)
attributes(x)

class(x)

z <- 0:9
print(z)
class(z)

digits <- as.character(z)
class(digits)

d <- as.integer(digits)
class(d)

f <- as.factor(digits)
class(f)

n <- as.numeric(digits)
class(n)

m <- as.matrix(digits)
class(m)


a <- as.array(digits)
class(a)

da <- as.data.frame(digits)
class(da)

#The data.frame object

name <- c("Amy", "Bill", "Carl")

DAD <- c(80, 65, 50)
BDA <- c(70, 50, 80)

gender <- as.factor(c("F", "M", "M"))
nationality <- as.factor(c("IRL", "UK", "IRL"))
age <- c(20, 21, 22)

student.df <- data.frame(name, age, gender, nationality, DAD, BDA)

attributes(student.df)

student.df['gender']

student.df$gender

student.df$gender[2]

mean(student.df$BDA)

student.df$average <- (student.df$BDA + student.df$DAD)/2
student.df

str(student.df)

student.df$name <- as.character(student.df$name)

student.df <- rbind(student.df, c("Dennis", 23, "M", "UK", 55, 70))

#1. Cast nationality to a character vector

student.df$nationality <- as.character(5)

#2. Add another 5-10 students, not all with the same nationality or gender

student.df <- rbind(student.df, c("Kenechukwu", 26, "M", "UK", 60, 70))

student.df <- rbind(student.df, c("Alochukwu", 24, "F", "IRL", 70, 70))

student.df <- rbind(student.df, c("Glory", 25, "F", "NGA", 80, 60))

student.df <- rbind(student.df, c("Akudo", 25, "F", "UK", 65, 75))

student.df <- rbind(student.df, c("Felix", 23, "M", "UK", 55, 85))

str(student.df)

student.df <- student.df[ , -7] # remove the average column
student.df$DAD <- as.numeric(student.df$DAD)
student.df$BDA <- as.numeric(student.df$BDA)
# now recompute the average for each student
student.df$average <- (student.df$BDA + student.df$DAD)/2
head(student.df)

#3. Rebuild the nationality factor (cast the nationality vector back to a factor)

student.df$nationality <- as.factor(c("IRL", "UK", "IRL", "UK", "UK", "IRL", "NGA", "UK", "UK"))

levels(student.df$nationality)

averages <- tapply(student.df$DAD, student.df$nationality, mean)
averages

table(student.df$nationality)

barplot(table(student.df$nationality), xlab = "Nationality", ylab="Count")

## compute the min, max, and standard deviation for BDA, and DAD

min(student.df$BDA)

max(student.df$BDA)

sd(student.df$BDA)

min(student.df$DAD)

max(student.df$DAD)

sd(student.df$DAD)


## build a data.frame of the results from 1 + mean

mins <- c(min(student.df$BDA), min(student.df$DAD))
means <- c(mean(student.df$BDA), mean(student.df$DAD))
maxs <- c(max(student.df$BDA), max(student.df$DAD))

sds <- c(sd(student.df$BDA), sd(student.df$DAD))
subjects <- data.frame(mins, maxs, means, sds, row.names = c("BDA", "DAD"))
names(subjects) <- c("min", "max", "mean", "sd")
print(subjects)

## Built in R datasets

data(mtcars) #loads the built-in dataset
str(mtcars) # shows us the structure of the dataset

summary(mtcars) # computes some basic descriptive information of the dataset

?mtcars

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am, labels=c("Automatic", "Manual"), levels=c(0,1))

str(mtcars)

summary(mtcars)

hist(mtcars$mpg, breaks = 10) #breaks controls how granular the histogram will be

boxplot(mtcars$hp)

barplot(table(mtcars$cyl))

coplot(mpg ~ hp | cyl, data = mtcars, panel = panel.smooth, rows = 1)
