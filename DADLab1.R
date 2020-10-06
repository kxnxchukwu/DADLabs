#Getting Help

help.start()

# Getting help on a specific function or to see an example of it in use, we use the ? symbol
?mean

?rnorm


# Using R to generate and visualise data
x <- rnorm(50)

y <- rnorm(x)

plot(x,y)

boxplot(x)

hist(x)

# To remove the variables from the workspace
x <- NULL

rm(y)


# Assigning numerical values to variables and manipulating them
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
# this is a comment
# and by the way, c(10.4, 5.6, 3.1, 6.4, 21.7) -> x would give the same result

x + 1

x - 1

x * 2

x / 2

log(x)

sqrt(x)

exp(x)

sin(x)

1/x

y <- 1/x

c(min(x), max(x))

length(x)

sum(x)

prod(x)

#Non-numeric data
temp <- x > 13

for (value in x){
  if (value >= 3 & value <= 7)
  {
    print('Values of x in between 3 and 7')
    print(value)
  }
  if (value > mean(x))
  {
    print('Value(s) greater than the mean of x are: ')
    print(value)
  }
}

m <- mean(x)
s <- sd(x)

x[x < (m - s) | x > (m + s)]

?Quotes

c("Hello", "World")

paste(c("X","Y"), 1:10, sep="")

#Missing Values
z <- c(1:3,NA)
is.na(z)

0/0

Inf - Inf

is.na(x)

is.nan(x)

#Indexes, selecting and modifying subsets of a data set
x <- c(-5:-1, NA, NA, 1:3)

y <- x[!is.na(x)]

y <- x[(!is.na(x)) & x>0]

y <- x[1:5]

y <- x[-(1:5)]

money <- c(10,100000,-10,NA,15)

strangeAnswers <- money[(is.na(x)) & money<0]

normalPeople <- money[(!is.na(x)) & money>=0 & money < 200]

toffs <- money[money > 10000]

fruit <- c(5, 10, 1, 20)

names(fruit) <- c("orange", "banana", "apple", "peach")

lunch <- fruit[c("apple","orange")]

x[is.na(x)] <- 0

x[is.na(x)] <- mean(x, na.rm=TRUE)
print(x)
