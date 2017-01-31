##Problem set 1 508##

setwd('C:/Users/hup11/desktop/Rworking')
digit <- read.csv ('digit_span_data.csv', header = TRUE)

##question 1
library(ggplot2)
##Setting mean
digit1 <- aggregate(digit$digit, by=list(digit$age), FUN=mean, na.rm = TRUE)

ggplot(data = digit1, aes(x=Group.1, y=x)) + geom_line() + labs (title = "Age and Mean Memory Span", x = "Age group", y = "Mean memory span(digit)") + scale_x_continuous (labels = c("4 yrs","8 yrs","12 yrs","16 yrs"))
  

##Question 1b.
attach(digit)
## linear relationship
digit$agelin[age==1] <- -3
digit$agelin[age==2] <- -1
digit$agelin[age==3] <-  1
digit$agelin[age==4] <-  3

## Quadratic relationship
digit$agequd[age==1] <-  1
digit$agequd[age==2] <- -1
digit$agequd[age==3] <- -1
digit$agequd[age==4] <-  1

## Cubic relationship
digit$agecub[age==1] <- -1
digit$agecub[age==2] <-  3
digit$agecub[age==3] <- -3
digit$agecub[age==4] <-  1


## Running analysis
lm2 <- lm(digit ~ agelin + agequd + agecub, data=digit)
summary(lm2)
anova(lm2)
library(ppcor)
pcor (digit[c("digit",'agelin','agequd','agecub')])

## Question 2. 
library(psych)
describe.by(digit$digit, digit$age)

##Question 2 d. 
attach(digit)
## Contrast 1
digit$age1[age==1] <- 0
digit$age1[age==2] <- 0
digit$age1[age==3] <- -1
digit$age1[age==4] <-  1

## Contrast 2
digit$age2[age==1] <-  0
digit$age2[age==2] <- -2
digit$age2[age==3] <-  1
digit$age2[age==4] <-  1

## Contrast 3
digit$age3[age==1] <- -3
digit$age3[age==2] <-  1
digit$age3[age==3] <-  1
digit$age3[age==4] <-  1

lm3 <- lm(digit ~ age1 + age2 + age3, data=digit)
summary(lm3)
anova(lm3)
library(ppcor)
pcor (digit[c("digit",'age1','age2','age3')])





