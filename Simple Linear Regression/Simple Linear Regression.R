# Simple Regression - Gokulakrishnan Selvarajan

#Installing necessary libraries
install.packages("pacman")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ISLR2")

#Loading libraries
pacman::p_load(pacman,ggplot2, ISLR2, gridExtra)

#Loading Auto Dataset
?Auto                #-------- Getting to know what is Auto Dataset
df_Auto <- Auto      #-------- Saving the dataset into a data frame
View(df_Auto)           #-------- Viewing the datset to understand


# 1.Using the Auto dataset: find the correlation between (mpg, horsepower), (mpg, weight), (mpg, acceleration)

#------(i) correlation between mpg and horsepower
cor_mpg_hp = cor(df_Auto$mpg, df_Auto$horsepower)
print(paste("The correlation between mpg and horsepower is", cor_mpg_hp))

#------(ii) correlation between mpg and weight
cor_mpg_wt = cor(df_Auto$mpg, df_Auto$weight)
print(paste("The correlation between mpg and weight is", cor_mpg_wt))

#------(iii) correlation between mpg and acceleration
cor_mpg_acc = cor(df_Auto$mpg, df_Auto$acceleration)
print(paste("The correlation between mpg and accelration is", cor_mpg_acc))


# 2. Graph a scatterplot for (mpg, horsepower), (mpg, weight) and (mpg, acceleration) and include a line of best fit

# (i) #------Scatter plot with confidence interval for (mpg, horsepower)
            
     hp_plot_without_ci <- ggplot(df_Auto, aes(x= horsepower , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "MPG vs Horsepower without CI", x = "Horsepower", y = "MPG")

   #------Scatter plot without confidence interval for (mpg, horsepower)
     hp_plot_with_ci <- ggplot(df_Auto, aes(x= horsepower , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + labs(title = "MPG vs Horsepower with CI", x = "Horsepower", y = "MPG")

   #------Scatter plot for (mpg, horsepower)
     grid.arrange(hp_plot_with_ci, hp_plot_without_ci, ncol = 2)
     
     
# (ii) #------Scatter plot with confidence interval for (mpg, weight)
     
     wt_plot_without_ci <- ggplot(df_Auto, aes(x= weight , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "MPG vs Weight without CI", x = "Weight", y = "MPG")
     
     #------Scatter plot without confidence interval for (mpg, weight)
     wt_plot_with_ci <- ggplot(df_Auto, aes(x= weight , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + labs(title = "MPG vs Weight with CI", x = "Weight", y = "MPG")
     
     #------Scatter plot for (mpg, weight)
     grid.arrange(wt_plot_with_ci, wt_plot_without_ci, ncol = 2)
     

# (iii) #------Scatter plot with confidence interval for (mpg, acceleration)
     
     acc_plot_without_ci <- ggplot(df_Auto, aes(x= acceleration , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "MPG vs Acceleration without CI", x = "Acceleration", y = "MPG")
     
     #------Scatter plot without confidence interval for (mpg, acceleration)
     acc_plot_with_ci <- ggplot(df_Auto, aes(x= acceleration , y = mpg)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + labs(title = "MPG vs Acceleration with CI", x = "Acceleration", y = "MPG")
     
     #------Scatter plot for (mpg, acceleration)
     grid.arrange(acc_plot_with_ci, acc_plot_without_ci, ncol = 2)
     
# 3. Describe the correlation in your own words. What types of relationships do you notice for each pair? Do you notice any similarities between the scatterplots?

#------ The correlation describes the strength of the relationship between two variables. The positive correlation denotes a direct relationship between two variables and negative correlation denotes inverse relationship.
#------ I notice negative correlation between (Horsepower vs MPG) & (Weight VS MPG). On the other hand, (Acceleration VS MPG) has positive correlation as one variable increases and other variable increases too.
#------ The scatter plots are very similar for (Horsepower vs MPG) & (Weight VS MPG) as they have a negative relationship.
     
# 4. Fit a simple linear regression model for: response (mpg), explanatory (horsepower)
     
reg_model_hp <- lm(mpg ~ horsepower, data = df_Auto)


# 5. Display the coefficients from the model
summary(reg_model_hp)   #---------- I am using summary instead of just coef to know about other values like std.error, t value and so on 


# 6. Repeat steps 4 and 5 for: response (mpg), explanatory (weight)  

reg_model_wt <- lm(mpg ~ weight, data = df_Auto)
summary(reg_model_wt)

# 7. If we increase the horsepower of a car by 1, what happens to mpg in our model?

#------ If we increase the horsepower by 1, the mpg of the car will decrease by about 0.157 units as they are inversely related to each other.

# 8. Predict the MPG for a car with 190 horsepower

Horsepower <- data.frame(horsepower=190)
ans <- predict(reg_model_hp, Horsepower)
print(paste("The MPG of the car with 190 horsepower will be",ans))

# 9. Predict the MPG for a car that weighs 4000lbs

Weight_1 <- data.frame(weight=4000)
ans_1 <- predict(reg_model_wt,Weight_1)
print(paste("The MPG of the car which weighs around 4000 lbs is",ans_1))
  