# Logistic Regression

heart <- read.csv("framingham.csv", header = T, strip.white = T)
head(heart)
str(heart)
summary(heart)

smokerNo <- subset(heart, currentSmoker == 0)
smokerYes <- subset(heart, currentSmoker == 1)
unique(smokerYes$cigsPerDay)
unique(smokerNo$cigsPerDay)

heart <- heart[!is.na(heart$cigsPerDay), ]
heart <- heart[!is.na(heart$BPMeds), ]
boxplot(heart$totChol, main = "Cholesterol Level")
heart$totChol[is.na(heart$totChol)] <- median(heart$totChol, na.rm = T)
boxplot(heart$BMI, main = "Body Mass Index (BMI)")
heart$BMI[is.na(heart$BMI)] <- median(heart$BMI, na.rm = T)
boxplot(heart$heartRate, main = "Heart Rate")
heart$heartRate[is.na(heart$heartRate)] <- median(heart$heartRate, na.rm = T)
boxplot(heart$glucose, main = "Glucose Level")
heart$glucose[is.na(heart$glucose)] <- median(heart$glucose, na.rm = T)

heart$male <- as.factor(heart$male)
heart$currentSmoker <- as.factor(heart$currentSmoker)
heart$BPMeds <- as.factor(heart$BPMeds)
heart$prevalentStroke <- as.factor(heart$prevalentStroke)
heart$prevalentHyp <- as.factor(heart$prevalentHyp)
heart$diabetes <- as.factor(heart$diabetes)
heart$TenYearCHD <- as.factor(heart$TenYearCHD)

str(heart)
summary(heart)


barplot(table(heart$male), ylim = c(0, 2500), col = c("#f4cccc", "#cfe2f3"), names.arg = c("Female", "Male"), 
        main = "Number of Female and Male Residents", ylab = "Number of Residents")
hist(heart$age, xlim = c(30, 70), ylim = c(0, 200), breaks = 39, col = "#87bba2", main = "Distribution of Residents' Age", xlab = "Age")
hist(heart$age, xlim = c(30, 70), col = "#c9e4ca", freq = F, main = "Distribution of Residents' Age", xlab = "Age")
lines(density.default(heart$age), col = "#f14e18", lwd = 4)
barplot(table(heart$currentSmoker), ylim = c(0, 2500), col = c("#4fc8d1", "#c76784"), names.arg = c("No", "Yes"), 
        main = "Smoking Status", ylab = "Number of Residents")
hist(heart$cigsPerDay, freq = F, col = "#c9e4ca", main = "Distribution of Cigarettes Consumed Per Day", ylab = "Frequency")
lines(density(heart$cigsPerDay), col = "#f14e18", lwd = 4)
barplot(table(heart$BPMeds), col = c("#4fc8d1", "#c76784"), names.arg = c("No", "Yes"), 
        main = "Blood Pressure Status", ylab = "Number of Residents")
barplot(table(heart$prevalentStroke), col = c("#4fc8d1", "#c76784"), names.arg = c("No", "Yes"), 
        main = "Stroke History Status", ylab = "Number of Residents")
barplot(table(heart$prevalentHyp), ylim = c(0, 3000), col = c("#4fc8d1", "#c76784"), names.arg = c("No", "Yes"), 
        main = "Hypertension History Status", ylab = "Number of Residents")
barplot(table(heart$diabetes), col = c("#4fc8d1", "#c76784"), names.arg = c("No", "Yes"), 
        main = "Diabetes History Status", ylab = "Number of Residents")
hist(heart$totChol, ylim = c(0, 0.01), col = "#c9e4ca", freq = F, main = "Distribution of Cholesterol Level", 
     xlab = "Cholesterol Level")
lines(density(heart$totChol), col = "#f14e18", lwd = 4)
hist(heart$sysBP, ylim = c(0, 0.025), col = "#c9e4ca", freq = F, main = "Distribution of Systolic Blood Pressure Level", 
     xlab = "Systolic Blood Pressure Level")
lines(density(heart$sysBP), col = "#f14e18", lwd = 4)
hist(heart$diaBP, ylim = c(0, 0.04), col = "#c9e4ca", freq = F, main = "Distribution of Diastolic Blood Pressure Level", 
     xlab = "Diastolic Blood Pressure Level")
lines(density(heart$diaBP), col = "#f14e18", lwd = 4)
hist(heart$BMI, ylim = c(0, 0.12), col = "#c9e4ca", freq = F, main = "Distribution of Body Mass Index (BMI)", 
     xlab = "BMI")
lines(density(heart$BMI), col = "#f14e18", lwd = 4)
hist(heart$heartRate, ylim = c(0, 0.05), col = "#c9e4ca", freq = F, main = "Distribution of Heart Rate", 
     xlab = "Heart Rate")
lines(density(heart$heartRate), col = "#f14e18", lwd = 4)
hist(heart$glucose, ylim = c(0, 0.06), col = "#c9e4ca", freq = F, main = "Distribution of Glucose Level", 
     xlab = "Glucose Level")
lines(density(heart$glucose), col = "#f14e18", lwd = 4)



model1 <- glm(formula = TenYearCHD ~ male + age + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp +
                diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, data = heart, family = binomial(link = "logit"))
summary(model1)


model2 <- glm(formula = TenYearCHD ~ male + age + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp +
                diabetes + totChol + sysBP + diaBP + heartRate + glucose, data = heart, family = binomial(link = "logit"))
summary(model2)


model3 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + 
                sysBP + diaBP + heartRate + glucose, data = heart, family = binomial(link = "logit"))
summary(model3)


model4 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + 
                sysBP + diaBP + glucose, data = heart, family = binomial(link = "logit"))
summary(model4)


model5 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + 
                sysBP + glucose, data = heart, family = binomial(link = "logit"))
summary(model5)


model6 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + totChol + sysBP + 
                glucose, data = heart, family = binomial(link = "logit"))
summary(model6)


model7 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose, 
              data = heart, family = binomial(link = "logit"))
summary(model7)


model8 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + sysBP + glucose, 
              data = heart, family = binomial(link = "logit"))
summary(model8)


model9 <- glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + sysBP + glucose, data = heart, 
              family = binomial(link = "logit"))
summary(model9)


diagnostic <- anova(model1, model2, model3, model4, model5, model6, model7, model8, model9, test = "Chisq")
diagnostic

aic <- AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9)
aic
