Consumer_Data <- read.table("C:/Users/manju/OneDrive/Data Analysis For Business Intelligence/Modules/Semester_1/Generalized linear models/Project/Credit_Score.txt")
Consumer_Data
Age = Consumer_Data[,1]
Income = Consumer_Data[,2]
Home = factor(Consumer_Data[,3])
Self_employed = factor(Consumer_Data[,4])
Number_of_insolvency = Consumer_Data[,5]
Application_status = factor(Consumer_Data[,6])
Application_status
#*********************************************
#modNull= glm(Home ~ 1, family = "binomial")
#summary(modNull)
#*********************************************
modAge= glm(Application_status~Age, family = "binomial")
summary(modAge)
modHome= glm(Application_status~Home, family = "binomial")
summary(modHome)
modInc= glm(Application_status~Income, family = "binomial")
summary(modInc)
modSE= glm(Application_status~Self_employed, family = "binomial")
summary(modSE)
modN= glm(Application_status~Number_of_insolvency, family = "binomial")
summary(modN)
modIncNum= glm(Application_status~Income+Number_of_insolvency, family = "binomial")
summary(modIncNum)
modAgeHome= glm(Application_status~Age+Home, family = "binomial")
summary(modAgeHome)
modAgeHomeSE= glm(Application_status~Age+Home+Self_employed, family = "binomial")
summary(modAgeHomeSE)
modAll= glm(Application_status~Home+Age+Income+Self_employed+Number_of_insolvency, family = "binomial")
summary(modAll)
modAge= glm(Application_status~Age, family = "binomial")
summary(modAge)
