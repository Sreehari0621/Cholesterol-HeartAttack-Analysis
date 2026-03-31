install.packages("readxl")
library(readxl)
data <- read_excel("heartchol.xlsx")

mean(data$age)
mean(data$chol)
mean(data$disease)
sd(data$age)
sd(data$chol)
table(data$disease)

mean(data$disease == 1)
mean(data$disease == 0)
high_chol <- data$chol > 240
table(high_chol)
mean(data$disease[high_chol] == 1)
mean(data$disease[!high_chol] == 1)

cor(data$age, data$chol)
cor(data$chol, data$disease)

model <- glm(disease ~ chol, data = data, family = "binomial")
summary(model)

table1 <- table(data$chol > 240, data$disease)
chisq.test(table1)

#Graph
hist(data$age, main="Age Distribution", xlab="Age")
hist(data$chol, main="Cholesterol Distribution", xlab="Chol")

barplot(table(data$disease), main="Heart Disease Count")

boxplot(chol ~ disease, data=data, main="Chol vs Disease")

plot(data$chol, jitter(data$disease),
     main="Chol vs Disease",
     xlab="Cholesterol",
     ylab="Disease")

plot(data$chol, jitter(data$disease),
     main="Logistic Regression Curve",
     xlab="Cholesterol",
     ylab="Disease Probability")

curve(predict(model, data.frame(chol=x), type="response"),
      add=TRUE, col="red", lwd=2)

