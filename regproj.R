#********************************************************************************************************************
#REGRESSION PROJECT LOAN PAYMENTS# By-Team GLM
#********************************************************************************************************************
######Packages#######
library(dplyr)
library(tidyverse)
library(MASS)
library(psych)
library(faraway)
library(car)
library(leaps)
library(MPV)
####################
#Read in data
loan<-read.csv("Regression sp20/Loan payments data.csv",header = T)
str(loan)

#Preliminary cleaning:
#convert to dates
loan$paid_off_time <- as.Date(loan$paid_off_time, format = "%m/%d/%Y")
loan$due_date <- as.Date(loan$due_date, format = "%m/%d/%Y")
loan$effective_date <- as.Date(loan$effective_date, format = "%m/%d/%Y")

#RESPONSE VARIABLE###
#New variable: so no need to include paid off time and effective date vars in model
loan$paid_off_time_minus_effective_date <- as.numeric(with(loan, paid_off_time - effective_date))

#fix education level:bachelors
levels(loan$education)<-c("Bachelor","college","High School or Below",
                         "Master or Above")
#Missing vals:
#Lets Replace NA's with 0 for past_due_days (How many days a loan has been past due) as they
#have already paid within the due date
loan$past_due_days[is.na(loan$past_due_days)]<-0

########################################
#MULTIPLE LINEAR REGRESSION w/ paid off time- effective date as response. 
########################################
#consider only paidoff subset, other wise its NA
loan.new<-filter(loan,loan_status=="PAIDOFF"|loan_status=="COLLECTION_PAIDOFF")
levels(loan.new$loan_status)<-c("COLLECTION_PAIDOFF" ,"COLLECTION_PAIDOFF","PAIDOFF" )

#pairs panel plot
pairs.panels(loan.new[,-c(1,5,7,8)])

#Graphs:
loan.new %>% ggplot(aes(age,(paid_off_time_minus_effective_date),color=loan_status))+geom_point()+geom_smooth(method = "lm",se=F)
loan.new %>% ggplot(aes(age,paid_off_time_minus_effective_date,color=Gender))+geom_point()+geom_smooth(method = "lm",se=F)
loan.new %>% ggplot(aes(age,paid_off_time_minus_effective_date,color=education))+geom_point()+geom_smooth(method = "lm",se=F)


#1 model initializaiton
attach(loan.new)
regloan0<-lm(paid_off_time_minus_effective_date~loan_status+Principal+terms+due_date+age+education+Gender,data = loan.new) 
#check vif
#vif(regloan0)#checking vif not really the case

#form model:
regloan<-lm(paid_off_time_minus_effective_date~loan_status+Principal+terms+due_date+age+education+Gender+
             age:loan_status+age:Gender+age:education,data = loan.new)
summary(regloan)
par(mfrow=c(2,2))
plot(regloan)#diagnostics
avPlots(regloan)#partial regression plot

#2.remedial mesure non-normality
bcox<-boxcox(paid_off_time_minus_effective_date~loan_status+Principal+terms+due_date+age+education+Gender+
               age:loan_status+age:Gender+age:education,
               data = loan.new[-c(6,209),],#won't accept 0  or negative values
               lambda = seq(-2, 2, length = 20)) # Compute and plot log-likelihoods for lambda in Box-Cox 
lambda<-bcox$x[bcox$y==max(bcox$y)]      # Return maximizing lambda for Box-Cox 0.58

#Apply power transformation on y=>sqrt, and fit again
regloan1<-lm(sqrt(paid_off_time_minus_effective_date)~loan_status+Principal+terms+due_date+age+education+Gender+
              age:loan_status+age:Gender+age:education+due_date:terms,data = loan.new)
summary(regloan1)
plot(regloan1)#check again

#3. variable selection
#Regsubset
regset<-regsubsets(sqrt(paid_off_time_minus_effective_date)~loan_status+Principal+terms+due_date+age+
                     education+Gender+
             age:loan_status+age:Gender+age:education+due_date:terms,data = loan.new)
plot(regset)
plot(regset,scale = "Cp")
plot(regset,scale = "adjr2")
plot(regset,scale = "r2")


##############Backward elimination###########################
w<-(update(regloan1,.~.-age:education,data=loan.new))
summary(w)          # Regression summary
anova(w)            # ANOVA summary
step(w,k=2)         # Computes AIC statistic
step(w,k=log(n))    # Computes BIC statistic
PRESS(w)    

w1<-(update(w,.~.-education,data=loan.new))
summary(w1)          # Regression summary
anova(w1)            # ANOVA summary
step(w1,k=2)         # Computes AIC statistic
step(w1,k=log(n))    # Computes BIC statistic
PRESS(w1)

w2<-(update(w1,.~.-age:Gender,data=loan.new))
summary(w2)          # Regression summary
anova(w2)            # ANOVA summary
step(w2,k=2)         # Computes AIC statistic
step(w2,k=log(n))    # Computes BIC statistic
PRESS(w2)

w3<-(update(w2,.~.-Gender,data=loan.new))
summary(w3)          # Regression summary
anova(w3)            # ANOVA summary
step(w3,k=2)         # Computes AIC statistic
step(w3,k=log(n))    # Computes BIC statistic
PRESS(w3)

w4<-(update(w3,.~.-age:loan_status,data=loan.new))
summary(w4)          # Regression summary
anova(w4)            # ANOVA summary
step(w4,k=2)         # Computes AIC statistic
step(w4,k=log(n))    # Computes BIC statistic
PRESS(w4)

w5<-(update(w4,.~.-age,data=loan.new))
summary(w5)          # Regression summary
anova(w5)            # ANOVA summary
step(w5,k=2)         # Computes AIC statistic
step(w5,k=log(n))    # Computes BIC statistic
PRESS(w5)
#########can stop here ########

#final model loan_status+principal+terms+due_date+[due_date*terms]#
w6<-w5
plot(w6)
#general lack of fit test. 
library(alr3)
pureErrorAnova(w6)#model is adequate!!

#non normality. box cox didnt fix so lets go with residual analysis 
#and removing influential points one by one

#4. model diagnostics

#Shapiro Francia Normality test:
library(nortest)
sf.test(w6$residuals)

############# Influential measure################
(rstudent(w6))
hatvalues(w6)        # Computes leverages
cooks.distance(w6)   # Computes Cook's D values
dffits(w6)           # Computes DfFits values.2236
dfbetas(w6)          # Computes DfBetas values


########### delete influential obs. one by one #################
#some observations: 1,6,46,64,82,88,136,148,153*,174,193, 209*,215, 245,259,279,293,309,337,357,,370, 373*,381*,387

w7=lm(sqrt(paid_off_time_minus_effective_date)~loan_status+Principal+terms+due_date+due_date:terms,data = loan.new[-c(64,259,136,209,293,373,387,370),])
#plot(w7)
#plot(lm(sqrt(paid_off_time_minus_effective_date)~loan_status+Principal+terms+due_date+due_date:terms,data = loan.new[-c(64,259,136,209,293,387,370,373),]))

#############################################

### Implement Robust Regression###***
bisquare.loan<-rlm(paid_off_time_minus_effective_date~loan_status+Principal+terms+due_date+due_date:terms,data = loan.new,
                   psi=psi.bisquare)
summary(bisquare.loan)
#round(confint.default(bisquare.loan),3)
#********************************************************************************************************************


#********************************************************************************************************************
# LOGISTIC REGRESSION updated after presentation. some results are different than ppt.
#********************************************************************************************************************

#Response variable: either paid or past due
loan$loan_status_binary <- as.factor(ifelse(loan$loan_status %in% c('PAIDOFF'), 1, 0))

# # create missing value flag #
# loan$missing_flag<-ifelse(is.na(loan$paid_off_time), 1, 0)
# loan$missing_flag<-as.factor(loan$missing_flag)

loan.imp<-loan
# # numeric impute: By mean #
# loan.imp$paid_off_time[is.na(loan.imp$paid_off_time)] <-mean(loan.imp$paid_off_time,na.rm = T)
# colSums(is.na(loan.imp))


# #######################################################################################
# Build full model try PURPOSEFUL  selection                                                               
newDataSet <- dplyr::select(loan.imp, -Loan_ID, -loan_status, -paid_off_time,  -past_due_days)
pairs.panels(newDataSet)

#step1Fit “simple” logistic regression models for each of the predictors separately.
#Eliminate any predictor values with large p-values (say >0.2).
summary(glm(loan_status_binary ~Principal, family=binomial, data=newDataSet))        #p=0.054
summary(glm(loan_status_binary ~terms, family=binomial, data=newDataSet))           #p=0.015673
summary(glm(loan_status_binary ~effective_date, family=binomial, data=newDataSet))   #p=8.94e-10
summary(glm(loan_status_binary ~due_date, family=binomial, data=newDataSet))         #p=0.0166
summary(glm(loan_status_binary ~age, family=binomial, data=newDataSet))              #p=0.672
summary(glm(loan_status_binary ~education, family=binomial, data=newDataSet))        #p=>0.6
summary(glm(loan_status_binary ~Gender, family=binomial, data=newDataSet))           #p=0.08723 


summary(glm(loan_status_binary ~Gender+Principal+terms+effective_date+due_date, family=binomial, data=newDataSet))           


#step2 Conduct forward/backward stepwise selection with remaining predictors,
#usually using a more stringent cut-off, such as p-value<0.1 or perhaps AIC/BIC. 

summary(glm(loan_status_binary ~ effective_date+due_date
            +Gender, family=binomial, data=newDataSet))

# step3 Consider adding in any variables that were not included in the model after Step 1 or Step 2.  
#A predictor can be added in even if p-value>0.1 if the AIC/BIC is lower or if it changes the estimated β coefficients by at least, say, 10%.

summary(glm(loan_status_binary ~ effective_date+due_date +terms
            +Gender, family=binomial, data=newDataSet))

summary(glm(loan_status_binary ~ effective_date+due_date +education
            +Gender, family=binomial, data=newDataSet))
summary(glm(loan_status_binary ~ effective_date+due_date +age
            +Gender, family=binomial, data=newDataSet))
summary(glm(loan_status_binary ~ effective_date+due_date +Principal
            +Gender, family=binomial, data=newDataSet))

# step4  Attempt adding plausible interactions among variables in the model, 
#usually using somewhat stricter standards such a p-value<0.05 (can consider non-linear predictor terms, like quadratic effects, in this step as well).

summary(glm(loan_status_binary ~ effective_date+due_date+Gender
            +effective_date:due_date, family=binomial, data=newDataSet))

summary(glm(loan_status_binary ~ effective_date+due_date+Gender
            +effective_date:Gender, family=binomial, data=newDataSet))

summary(glm(loan_status_binary ~ effective_date+due_date+Gender
            +Gender:due_date, family=binomial, data=newDataSet))

#best model for PURPOSEFUL selection  # AIC 612.8
bestmodel <- glm(loan_status_binary ~ effective_date+due_date+Gender, family=binomial, data=newDataSet)


# Using Backward selection we get more strictfor  p<0.5, predictors included effective_date and due_date
full <- glm(loan_status_binary ~., family=binomial, data=newDataSet)
summary(full)
n<-length(loan.imp$loan_status_binary)

reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)     #AIC: 621.83
############################################################################


# For checking ROC Curve of the model
library(pROC) 
library(ROCR)
rocplot3 <- roc(newDataSet$loan_status_binary ~ fitted(bestmodel)) 
plot.roc(rocplot3, print.auc = TRUE)
auc(rocplot3)   # Area under the curve: 0.6931


#
ptest <- as.numeric(bestmodel$fitted>0.5)                        # Binary vector set to 1 if the prob. exceeds 0.50 and 0 otherwise
cnf_matrix = table(predicted=ptest, actual=newDataSet$loan_status_binary)                # Contingency table of test outcomes
cnf_matrix
TN = cnf_matrix[1,1] # True Negative - Actual & Predicted is 0/N
TP = cnf_matrix[2,2] # True Positive - Actual & Predicted is 1/Y
FP = cnf_matrix[2,1] # False Positive - Actual is 0/N but Predicted is 1/Y 
FN = cnf_matrix[1,2] # False Nefgative - Actual is 1/Y but Predicted is 0/N
TO = TN+TP+FP+FN # Total Observations

accuracy = (TP+TN)/TO # Accuracy or Prevalance of Confusion Matrix = 0.63
accuracy
precision = TP/(TP+FP) # Precision = 0.68
precision
sensitivity = TP/(TP+FN) # True Positive Rate = 0.71
sensitivity
error = (FP+FN)/TO # Error Rate = 0.37
error
specificity = TN/(TN+FP)  #0.5
specificity
#G-mean
G=sqrt(specificity*sensitivity)  #G mean= 0.59
G     

############################################################################

library(glmnet)
x<-cbind(newDataSet$Principal, newDataSet$terms, newDataSet$due_date,
         newDataSet$age, newDataSet$education, newDataSet$Gender, newDataSet$effective_date)  # Combine variables by column
y<-newDataSet$loan_status_binary
grid<-10^seq(10,-2, length=100)        # Create a grid of lambda values

lasso.mod=cv.glmnet(x,y,lambda=grid,   # Build a CV ridge regression          
                    nfold=length(y),                       # nfold=sample size, leave-one-out CV
                    alpha=1, family='binomial')      # alpha=0, ridge reg is fit


##Ignore the received warning which recommends leaving 3-or-more out in CV ## 
#Warning message:
#Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold 

plot(log10(lasso.mod$lambda), lasso.mod$cvm,      # Plot average CV error versus log(lambda)
     xlab="log10(Lambda)", ylab="CV Error") 
abline(v = log10(lasso.mod$lambda.min), lty = 3)

(lambda=lasso.mod$lambda.min)        # The lambda that minimizes CV error

predict(lasso.mod,s=lambda,          # Obtain ridge reg coefs
        type="coefficients")         #####principal, due date, gender, effective date#########

fit123 = predict(lasso.mod, newx = x ,newdata = newdata, type = 'response') 
# If results are more than 50% then convert to 1 else 0
fit123 = ifelse(fit123 >=0.5,1,0) #Setting cut-off to be at 0.5
# Evaluate predictions on the training dataset through Confusion Matrix
cnf_matrix2 = table(predicted = fit123, actual = newDataSet$loan_status_binary)
cnf_matrix2

TN = cnf_matrix2[1,1] # True Negative - Actual & Predicted is 0/N
TP = cnf_matrix2[2,2] # True Positive - Actual & Predicted is 1/Y
FP = cnf_matrix2[2,1] # False Positive - Actual is 0/N but Predicted is 1/Y 
FN = cnf_matrix2[1,2] # False Nefgative - Actual is 1/Y but Predicted is 0/N
TO = TN+TP+FP+FN # Total Observations

accuracy2 = (TP+TN)/TO # Accuracy or Prevalance of Confusion Matrix = 0.618
accuracy2
precision2 = TP/(TP+FP) # Precision = 0.625
precision2
sensitivity2 = TP/(TP+FN) # True Positive Rate = 0.90
sensitivity2
error2 = (FP+FN)/TO # Error Rate = 0.382
error2
specificity2 = TN/(TN+FP)  #0.185
specificity2
#G-mean
G=sqrt(specificity*sensitivity2)  #G mean= 0.67
G 








