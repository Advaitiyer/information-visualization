###########################
# Name: Advait Ramesh Iyer
###########################
# Data downloaded from the following link: https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset/downloads/ibm-hr-analytics-employee-attrition-performance.zip/1

fname <- file.choose()
employees <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
View(employees)
str(employees)

new_df <- data.frame(employees$Age, employees$DailyRate
                     , employees$DistanceFromHome, employees$HourlyRate
                     , employees$MonthlyIncome, employees$MonthlyRate
                     , employees$NumCompaniesWorked, employees$PercentSalaryHike
                     , employees$TotalWorkingYears, employees$YearsAtCompany
                     , employees$YearsInCurrentRole, employees$YearsSinceLastPromotion
                     , employees$YearsWithCurrManager)
colnames(new_df) <- c("Age", "Daily Rate", "Distance From Home", "Hourly Rate"
                      , "Monthly Income", "Monthly Rate", "Num Companies Worked"
                      , "Percent Salary Hike", "Total Working Years", "Years at Company"
                      , "Years in Current Role", "Years Since last Promotion"
                      , "Years with Current Manager")

# Plot1: Salary by Job Level and Education
par(mar = c(6,6,4,2))

salary.joblevel.education <- tapply(X=employees$MonthlyIncome
                           , INDEX = list(employees$JobLevel
                           ,employees$Education), FUN = sum)
salary.joblevel.education
plot1 <- barplot(salary.joblevel.education, beside = T, las=1
                 , main="Salary by Job Level and Education"
                 , ylab = "Salary", ylim = c(0,1000000), xlab = "Education Level"
                 , col = c("#1E2E41","#37337C","#2B4D74","#247361","#44FFd5")
                 , names.arg = c("Below College","College","Bachelor"
                                 ,"Master","Doctor"))
legend("topright",c("1","2","3","4","5")
       , fill = c("#1E2E41","#37337C","#2B4D74","#247361","#44FFd5"))

mtext("Source: Kaggle", side = 1
            , line = 3, at = 30, cex = 0.85)
mtext("Job Level", side = 3
      , line = 0.1, at = 29.75, cex = 0.9)

#Plot2:
Years_at_company <- employees$YearsAtCompany
Years_current_role <- employees$YearsInCurrentRole
Years_promotion <- employees$YearsSinceLastPromotion
Years_with_manager <- employees$YearsWithCurrManager
Total_working_years <-  employees$TotalWorkingYears
par(mar=c(5,9.5,3,2))
boxplot(Years_at_company, Years_current_role, Years_promotion, Years_with_manager,Total_working_years
        , main = "Comparison of Retention Metrics", at = c(1,2,3,4,5)
        , names = c("Years_Company","Years_Curr_Role","Years_Promotion"
                  , "Years_Curr_Manager","Total_Years"), las = 2
        , col = c("darkred","red","tomato","orange","yellow"), border = "brown"
        , horizontal = TRUE, notch = TRUE, xlab = "Number of Years")

mtext("Source: Kaggle", side = 1
      , line = 3, at = 40, cex = 0.85)

#Plot3:
par(mfrow = c(1,1))
plot3 <- hist(employees$DailyRate
              , ylab = "Frequency", col = "turquoise"
              , xlim = c(0,1600), ylim = c(0,140)
              , main = "Histogram of Daily Rates")
mtext("Source: Kaggle", side = 1
      , line = 2, at = 1500, cex = 0.7)

plot4 <- hist(employees$HourlyRate, xlab = "Hourly Rate"
              , ylab = "Frequency", col = "thistle1"
              , ylim = c(0,120), main = "Histogram of Hourly Rates")
mtext("Source: Kaggle", side = 1
      , line = 2, at = 100, cex = 0.7)


plot5 <- hist(employees$MonthlyIncome, xlab = "Monthly Income"
              , ylab = "Frequency", main = "Histogram of Monthly Income"
              , col = "springgreen", ylim = c(0,600))
mtext("Source: Kaggle", side = 1
      , line = 2, at = 20000, cex = 0.7)


  
  
  
  
  
  
