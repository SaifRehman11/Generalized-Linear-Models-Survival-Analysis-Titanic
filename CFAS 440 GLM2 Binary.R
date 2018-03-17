library(MASS)
library(car)
library(ggplot2)
titanic <- read.table("titanic.txt", header = TRUE)

sum(is.na(titanic)) #Total of 557 NA entries
sum(is.na(titanic$Age)) #All the NA's are for Age values




titanic <- na.omit(titanic)
dim(titanic) #756 rows remain after removing the NA values
summary(titanic)

plot(titanic)
attach(titanic)

plot(PClass, Survived, main = "Survived plotted against Passenger Class", xlab = "Passenger Class", ylab = "Survived")
plot(Sex, Survived, main = "Survived plotted against Sex", xlab = "Sex", ylab = "Survived")
plot(Age, Survived, main = "Passnger Class plotted against Survived", xlab = "Passenger Class", ylab = "Survived")
scatterplot(Age, Survived, main = "Survived plotted against Age", xlab = "Age", ylab = "Survived")
scatterplot(Sex, Survived)
boxplot(Sex, Survived)
scatterplot(PClass, Survived)
plot(PClass, Sex, main = "Sex against Passenger Class", xlab = "Passenger Class", ylab = "Sex")
plot(Sex, Age, main = "Age against Sex", xlab = "Sex", ylab = "Age")

fit0 <- glm(Survived ~ 1)
summary(fit0)

titanic$Dead <- abs(1- titanic$Survived)
y <- cbind(titanic$Survived, titanic$Dead)
mod1 <- glm(y ~ PClass + Age + Sex, data = titanic , family = binomial)
summary(mod1)

fit1 <- glm(Survived ~ PClass + Age + Sex, data = titanic , family = binomial)
summary(fit1)
Anova(fit1)
cbind(coef(fit1), exp(coef(fit1)))

scatterplot(Age, fitted(fit1), groups = titanic$PClass)

ggplot(titanic) + geom_point(aes(titanic$Age, fitted(fit1), shape = titanic$Sex, colour = titanic$PClass, show.legend = TRUE)) + geom_smooth(aes(x = titanic$Age, y = fitted(fit1),method = "lm", se = TRUE, linetype = titanic$PClass, show.legend = TRUE))

ggplot(titanic, aes(titanic$Age, fitted(fit1), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)

#This one works the best
ggplot(titanic, aes(titanic$Age, fitted(fit1), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE) + labs(title = "Fit 1 Scatterplot", x = "Age", y = "Fitted values for Fit 1")


#p = Survived / (Survived + titanic$Dead)

#plot(Age, p)
#legend(x="topright", legend = levels(titanic$PClass), col = c("red", "blue", "green"), pch=1)
#legend(x="topleft", legend = levels(titanic$Sex), col = c("red", "blue"), pch=2)
1-pchisq(695.14, 751)

1-pchisq(113.05,751)


fit2 <- glm(Survived ~ PClass + I(Age^2) + Sex, data = titanic, family = binomial)
summary(fit2)
Anova(fit2)
scatterplot(Age, fitted(fit2))
ggplot(titanic, aes(titanic$Age, fitted(fit2), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)


fit3 <- glm(Survived ~ PClass + Age + I(Age^2) + Sex, data = titanic, family = binomial)
summary(fit3)
Anova(fit3)
scatterplot(Age, fitted(fit3))

ggplot(titanic, aes(titanic$Age, fitted(fit3), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)

ggplot(titanic, aes(titanic$Age, fitted(fit3), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE) + labs(title = "Fit 3 Scatterplot", x = "Age", y = "Fitted values for Fit 3")


mod2 <- glm(y ~ PClass + Age + I(Age^2) + Sex, data = titanic , family = binomial)
summary(mod2)
Anova(mod2)

cbind(coef(fit2), exp(coef(fit2)))
cbind(coef(fit1), exp(coef(fit1)))


fit4 <- glm(Survived ~ PClass + Age*Sex, data = titanic, family = binomial)
summary(fit4)
Anova(fit4)
scatterplot(Age, fitted(fit4))
ggplot(titanic, aes(titanic$Age, fitted(fit4), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)

cbind(coef(fit4), exp(coef(fit4)))

fit5 <- glm(Survived ~ PClass* Age *Sex, data = titanic, family = binomial)
summary(fit5)
Anova(fit5)
scatterplot(Age, fitted(fit5))
ggplot(titanic, aes(titanic$Age, fitted(fit5), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)

cbind(coef(fit5), exp(coef(fit5)))

fit6 <- glm(Survived ~ PClass + Age + Sex + PClass:Age + PClass:Sex + Age:Sex, data = titanic, family = binomial)
summary(fit6)
Anova(fit6)
scatterplot(Age, fitted(fit6))

finalresid <- rstandard(fit6)
qqnorm(finalresid)
qqline(finalresid)

1-pchisq(641.37, 746)

ggplot(titanic, aes(titanic$Age, fitted(fit6), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE)

ggplot(titanic, aes(titanic$Age, fitted(fit6), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE) + labs(title = "Fit 6 Scatterplot", x = "Age", y = "Fitted values for Fit 6")


cbind(coef(fit6), exp(coef(fit6)))

test <- read.table("titanic.txt", header = TRUE)
test <- na.omit(test)

test$model <- round(predict(fit1, titanic, type = "response"))
nrow(test[test$model == test$Survived,])
nrow(test[test$model == test$Survived,])/nrow(test)


test <- read.table("titanic.txt", header = TRUE)
test <- na.omit(test)
dim(test)

test$model <- round(predict(fit6, titanic, type = "response"))
nrow(test[test$model == test$Survived,])
nrow(test[test$model == test$Survived,])/nrow(test)
#fit7 <- glm(Survived ~ PClass + Age + Sex + PClass:Sex + Age:Sex, data = titanic, family = binomial)
#summary(fit7)
#Anova(fit7)
#scatterplot(Age, fitted(fit7))
#ggplot(titanic, aes(titanic$Age, fitted(fit7), colour = titanic$PClass)) + geom_point(aes(shape = titanic$Sex)) + geom_smooth(method = "lm", se = TRUE, span = 0.8)

#cbind(coef(fit7), exp(coef(fit7)))

#finalresid <- rstandard(fit7)
#qqnorm(finalresid)
#qqline(finalresid)


