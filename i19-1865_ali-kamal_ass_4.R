library(psych)
df <- read.csv("C:/Users/Ali/Desktop/Assignment 4/heart_failure_clinical_records_dataset .csv")
alpha=0.05

#Question 1. Are diabetic peoples being more likely to have Blood Pressure?
#H0:- Diabetic People and Blood Pressure are independent
#Ha:- Diabetic People and Blood Pressure are not independent
pval=chisq.test(df$diabetes,df$high_blood_pressure,correct=TRUE)$p.value
print(paste("Since P value",pval,"is > alpha value",alpha,", we fail to reject H0,",
            "and conclude that the events are independent, hence diabetic people are",
            "not more likely to have Blood Pressure"))



#Question 2. Are people having anemia are more likely to be from female?
#H0:- Anemia and gender are independent
#Ha:- Anemia and gender are not independent
pval=chisq.test(df$anaemia,df$sex,correct=TRUE)$p.value
print(paste("Since P value",pval,"is > alpha value",alpha,", we fail to reject H0,",
            "and conclude that the events are independent, hence people having anemia",
              "are not more likely to be from female"))



#Question 3. Are people whose age is in between 55 and 65 having high Blood Pressure are less likely to survive?
#H0:- High Blood Pressure and Death Event are independent for people in ages 55-65
#Ha:- High Blood Pressure and Death Event are not independent for people in ages 55-65
df1=df[df$age>=55 & df$age<=65,]
pval=chisq.test(df1$high_blood_pressure,df1$DEATH_EVENT,correct=FALSE)$p.value
corr=tetrachoric(table(df1$high_blood_pressure,df1$DEATH_EVENT))$rho
print(paste("Since P value",pval,"is < alpha value",alpha,", we reject H0,",
            "and conclude that the events are dependent. I then performed correlation test for binary",
            "categorical variables (high blood pressure and death event) using tetrachoric correlation,",
             "which gives value of",corr,"which shows a slight negative correlation between high blood pressure and",
             "death event, which shows that people having age between 55 and 65 having high Blood Pressure are",
             "MORE LIKELY to survive"))



#Question 4. Are diabetic peoples who smokes are less likely to survive
#H0:- Smoking and death event is independent for diabetic people
#Ha:- Smoking and death event is not independent for diabetic people
df1=df[df$diabetes==1,]
pval=chisq.test(df1$smoking,df1$DEATH_EVENT, correct=TRUE)$p.value
print(paste("Since P value",pval,"is > alpha value",alpha,", we fail to reject H0,",
            "and conclude that the events are independent, hence diabetic people who smoke",
            "are not less likely to be survive"))



#Question 5. Are male people being more likely to get diabetic?
#H0:- Gender and diabetes are independent
#Ha:- Gender and diabetes are not independent
pval=chisq.test(df$sex,df$diabetes,correct=TRUE)$p.value
corr=tetrachoric(table(df$sex,df$diabetes))$rho
print(paste("Since P value",pval,"is < alpha value",alpha,", we reject H0,",
            "and conclude that the events are dependent. I then performed correlation test for binary",
            "categorical variables (gender and diabetes) using tetrachoric correlation,",
            "which gives value of",corr,"which shows a slight negative correlation between gender and",
            "diabetes, which shows that males are less likely to have diabetes"))
