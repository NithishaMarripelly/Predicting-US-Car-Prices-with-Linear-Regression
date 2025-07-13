library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(leaps)

#----------------------------Reading dataset-------------------------
Cars <- read.table("C:/Users/nithi/OneDrive/Desktop/SEM_II/linearRegression/Project/CarPrice_Assignment1.csv", header=TRUE,sep = ",")
Cars

Cars <- Cars[, -1]
names(Cars)

View(unique(Cars$CarName))

#----------------------------Extract the company name from the 'CarName' column and create a new 'Company' column
Cars <- Cars %>%
  mutate(Company = str_extract(CarName, "^[^ ]+"))


#----------------------------Print the unique values in the 'Company' column after cleaning---------------------------

unique(Cars$Company)


#----------------------------Replace specific company names---------------------------------

Cars <- Cars %>%
  mutate(Company = case_when(
    Company == "alfa-romero" ~ "alfa-romeo",
    Company == "maxda" ~ "mazda",
    Company == "porcshce" ~ "porsche",
    Company == "toyouta" ~ "toyota",
    Company == "vokswagen" ~ "volkswagen",
    Company == "vw" ~ "volkswagen",
    TRUE ~ Company
  ))

Cars <- Cars %>%
  mutate(BrandType = str_extract(Company, "^[^ ]+"))

Cars <- Cars %>%
  mutate(BrandType = case_when(
    BrandType == "alfa-romeo" ~ "luxury",
    BrandType == "audi" ~ "luxury",
    BrandType == "bmw" ~ "luxury",
    BrandType == "jaguar" ~ "luxury",
    BrandType == "mercedes-benz" ~ "luxury",
    BrandType == "porsche" ~ "luxury",
    BrandType == "volvo" ~ "luxury",
    BrandType == "mercury" ~ "luxury",
    BrandType == "buick" ~ "luxury",
    BrandType == "chevrolet" ~ "Midrange",
    BrandType == "dodge" ~ "Midrange",
    BrandType == "honda" ~ "Midrange",
    BrandType == "isuzu" ~ "Midrange",
    BrandType == "mazda" ~ "Midrange",
    BrandType == "mitsubishi" ~ "Midrange",
    BrandType == "nissan" ~ "Midrange",
    BrandType == "Nissan" ~ "Midrange",
    BrandType == "peugot" ~ "Midrange",
    BrandType == "peugeot" ~ "Midrange",
    BrandType == "plymouth" ~ "Midrange",
    BrandType == "renault" ~ "Midrange",
    BrandType == "saab" ~ "Midrange",
    BrandType == "subaru" ~ "Midrange",
    BrandType == "toyota" ~ "Midrange",
    BrandType == "volkswagen" ~ "Midrange",
    TRUE ~ BrandType
  ))


#-----------------------Drop the 'CarName' column now that 'Company' has been extracted-----------------------------------

Cars <- Cars %>%
  select(-CarName)



#-----------------------Drop the 'CarName' column now that 'Company' has been extracted------------------------

Cars <- Cars %>%
  select(-Company)


#-----------------------Print the unique values in the 'Company' column after cleaning-------------------

unique(Cars$BrandType)



#-----------------------to identify the columns as categorical while calculation------------------------------

Cars$symboling <- factor(Cars$symboling)
Cars$BrandType <-factor(Cars$BrandType)
Cars$enginelocation <-factor(Cars$enginelocation)
Cars$cylindernumber <-factor(Cars$cylindernumber)
Cars$fuelsystem <-factor(Cars$fuelsystem)




#-----------------------creating dummyvariables for categorical predictors--------------------------

library(fastDummies)
Cars_with_dummy <- dummy_cols(Cars, select_columns = c("enginelocation","symboling", "BrandType", "enginlocation", "cylindernumber", "fuelsystem"), remove_selected_columns = TRUE, remove_first_dummy =  TRUE)
View(Cars_with_dummy)



#-----------------------Centering--------------------------------------------------------------

continuous <- Cars_with_dummy[,1:12]
centered <- data.frame(matrix(nrow = 205))
for (col in names(continuous)) {  # Iterate through column names
  centered[, col] <- continuous[, col] - mean(continuous[, col])
}
centered <- select(centered, -1)
View(centered)


#------------------------------Squared terms---------------------------------------------

squared <- data.frame(matrix(nrow = 205))
for (col in names(continuous)) {  # Iterate through column names
  squared[, paste0(col, "2")] <- (continuous[, col] - mean(continuous[, col]))*(continuous[, col] - mean(continuous[, col]))
}
squared <- select(squared, -1)




#----------------------------remove price from cars with dummy and store data in new dataframe-----------

cwd_no_i <- Cars_with_dummy %>%
  select(-price)
View(cwd_no_i)



#-----------------------------remove continuous (first 13columns)-----------------------------------

cwd_no_i <- select(cwd_no_i, 13:ncol(cwd_no_i))
View(cwd_no_i)



#--------------------------add centered terms for interaction------------------------------------------------

pre_interaction <- cbind(centered,cwd_no_i)



#--------------------------Interaction Terms------------------------------------------------------

interaction_terms <- as.data.frame(model.matrix(~ (.-1)^2, data = pre_interaction))



#--------------------------add price nd squared terms----------------------------------------------

interaction_terms$price <- Cars_with_dummy$price
all_terms <- cbind(squared,interaction_terms)


#--------------------------adding price column to centered--------------------------------------------

centered$price<-Cars_with_dummy$price
View(centered)

#correlation plots
library("ggplot2")
library("GGally")
ggpairs(centered)

#---------------------------Split the data into 90/10 train/test split-----------------------------

set.seed(30)  # For reproducibility
train_index <- createDataPartition(all_terms$price, p = 0.8, list = FALSE)
train_data <- all_terms[train_index, ]
test_data <- all_terms[-train_index, ]


#--------------------------Validation of split-----------------------------------------------------------

fwd_final1 <- lm(price ~ wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+enginelocation_rear+symboling_Safe+BrandType_Midrange+cylindernumber_NormalPerf+fuelsystem_FuelInjected, data = train_data)
summary(fwd_final1)

fwd_final2 <- lm(price ~ wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+enginelocation_rear+symboling_Safe+BrandType_Midrange+cylindernumber_NormalPerf+fuelsystem_FuelInjected, data = test_data)
summary(fwd_final2)



#--------------------------define base, null and full model-----------------------------------------

model_with_interaction <- lm(price~., data = all_terms)
basemodel <- lm(price~curbweight+enginesize+horsepower, data = all_terms)


summary(model_with_interaction)


# -------------------MODEL BUILDING USING FORWARD SELECTION-----------------

forward.model_with_interactions.aic <- step(basemodel, direction = "forward", scope = list(upper = model_with_interaction, lower = basemodel))
summary(forward.model_with_interactions.aic)

modelfwd <- regsubsets(price ~ curbweight + enginesize + horsepower + `enginesize:BrandType_Midrange` + 
                         BrandType_Midrange + `curbweight:enginelocation_rear` + `carheight:enginesize` + 
                         `stroke:compressionratio` + `curbweight:symboling_Safe` + 
                         `boreratio:cylindernumber_NormalPerf` + carheight2 + `citympg:fuelsystem_FuelInjected` + 
                         `peakrpm:citympg` + `stroke:BrandType_Midrange` + `enginesize:compressionratio` + 
                         `horsepower:BrandType_Midrange` + boreratio + `BrandType_Midrange:cylindernumber_NormalPerf` + 
                         cylindernumber_NormalPerf + `carlength:horsepower` + `carwidth:symboling_Safe` + 
                         `carheight:boreratio` + `wheelbase:enginesize` + `horsepower:cylindernumber_NormalPerf` + 
                         horsepower2 + `carheight:peakrpm` + `boreratio:peakrpm` + 
                         `enginesize:peakrpm` + `compressionratio:symboling_Safe` + 
                         `compressionratio:BrandType_Midrange` + `compressionratio:citympg` + 
                         carlength + `carlength:fuelsystem_FuelInjected` + `curbweight:boreratio` + 
                         `carlength:boreratio` + `wheelbase:peakrpm` + `carwidth:fuelsystem_FuelInjected` + 
                         `stroke:peakrpm` + `curbweight:enginesize` + `carlength:compressionratio` + 
                         `wheelbase:carwidth` + `carlength:stroke` + `curbweight:peakrpm` + 
                         `citympg:BrandType_Midrange` + `curbweight:stroke` + `carheight:stroke` + 
                         compressionratio2 + `wheelbase:boreratio` + `carheight:symboling_Safe` , data = all_terms, nbest = 1)


leaps.fwd <- summary(modelfwd)
result.fwd <- with(leaps.fwd, round(cbind(which, rsq, adjr2, cp, bic), 3))
result.fwd

fwd.final <- lm(price~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+`curbweight:enginelocation_rear`+`stroke:compressionratio`+`enginesize:compressionratio`+`horsepower:BrandType_Midrange`, data = train_data)
summary(fwd.final)

#-------------------------slope plots for interaction terms------------------------------------------

plot(train_data$`enginesize:BrandType_Midrange`, train_data$price)
abline(lm(price~enginesize:BrandType_Midrange, data = train_data))

plot(train_data$`curbweight:enginelocation_rear`, train_data$price)
abline(lm(price~curbweight:enginelocation_rear, data = train_data))

plot(train_data$`stroke:compressionratio`, train_data$price)
abline(lm(price~stroke:compressionratio, data = train_data))

plot(train_data$`enginesize:compressionratio`, train_data$price)
abline(lm(price~enginesize:compressionratio, data = train_data))

plot(train_data$`horsepower:BrandType_Midrange`, train_data$price)
abline(lm(price~horsepower:BrandType_Midrange, data = train_data))

#--------------------------VIF calculation--------------------------------------------------------

fwd_final <- lm(price~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+`curbweight:enginelocation_rear`+enginelocation_rear + `stroke:compressionratio`+stroke+compressionratio+`enginesize:compressionratio`+`horsepower:BrandType_Midrange`+horsepower, data = train_data)
summary(fwd_final)
library(car)
vif_fwd <- vif(fwd_final)
vif_fwd


plot(train_data$`enginelocation_rear`, train_data$price)
abline(lm(price~enginelocation_rear, data = train_data))

plot(train_data$`curbweight:enginelocation_rear`, train_data$price)
abline(lm(price~curbweight:enginelocation_rear, data = train_data))
# both the above plots are identical, so removing curbweight:enginelocation_rear

#----------------------removed from model because of slope----------------------------------

fwd_final1_1 <- lm(price~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear + `stroke:compressionratio`+stroke+compressionratio+`enginesize:compressionratio`+`horsepower:BrandType_Midrange`+horsepower, data = train_data)
summary(fwd_final1_1)
vif_fwd1 <- vif(fwd_final1_1)
vif_fwd1

#----------------------deleted interaction terms based on VIF-----------------------------------

fwd_final1_2 <- lm(price~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear + `stroke:compressionratio`+stroke+compressionratio+`enginesize:compressionratio`, data = train_data)
summary(fwd_final1_2)
vif_fwd_2 <- vif(fwd_final1_2)
vif_fwd_2

#---------------------------------------FINAL MODEL----------------------------------------------------

fwd_final1_3 <- lm(price~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear + `stroke:compressionratio`+stroke+compressionratio+`enginesize:compressionratio`, data = train_data)
summary(fwd_final1_3)


#----------------outlier analysis wrt Y-------------------------------------

no<-length(train_data$price)
X_0<-rep(1,n)
X<-as.matrix(cbind(X_0,train_data$curbweight,train_data$enginesize,train_data$horsepower,train_data$BrandType_Midrange,train_data$`enginesize:BrandType_Midrange`))
X
Y<-as.matrix(train_data$price,ncol=1)
p<-ncol(X)
XtX<-t(X)%*%X
XtX_inv<-solve(XtX)
XtY<-t(X)%*%Y
b<-XtX_inv%*%XtY
yhat<-X%*%b
e<-Y-yhat


#-------------hat matrix------------------

H<-X%*%XtX_inv%*%t(X)
J_n<-matrix(rep(1/no,no^2),ncol=no)
I_n<-diag(no)
SSE<-t(Y)%*%(I_n-H)%*%Y
MSE<-SSE/(no-p)
s2_e<-MSE[1,1]*(I_n-H)
s_e<-sqrt(diag(s2_e))
s_e
diag(H)
e_star<-e/sqrt(MSE[1,1])
r<-e/sqrt(MSE[1,1]*(1-diag(H)))
d<-e/(1-diag(H))
t<-e*sqrt((n-p-1)/(SSE[1,1]*(1-diag(H))-e^2))
t
alpha<-0.05
qt(1-alpha/(2*n),n-p-1)
table<-cbind(train_data$curbweight,train_data$enginesize,train_data$horsepower,train_data$BrandType_Midrange,train_data$`enginesize:BrandType_Midrange`,Y,yhat,e,e_star,r,d,abs(t)-qt(1-alpha/(2*n),n-p-1))
colnames(table)<-cbind("curbweight","enginesize","horsepower","BrandType_Midrange","enginesize:BrandType_Midrange","Y","yhat","e","e_star","r","d","abs(t)-qt(1-alpha/(2*n),n-p-1)")
print(table,quote=FALSE)
table<-as.data.frame(table)
table[table$"abs(t)-qt(1-alpha/(2*n),n-p-1)">0,]


#----------OUTLIERS WRT TO X----------------------------------------

Leverage_test<- diag(H)-(2*p/n)*X_0
Leverage_test
LT<- cbind(train_data$curbweight,train_data$enginesize,train_data$horsepower,train_data$BrandType_Midrange,train_data$`enginesize:BrandType_Midrange`,Y, diag(H), Leverage_test)
colnames(LT)<- cbind("curbweight", "enginesize", "horsepower", "BrandType_Midrange", "enginesize:BrandType_Midrange", "Y", "h_ii", "h_ii-2p/n")
LT <- as.data.frame(LT)
LT[LT$"h_ii-2p/n">0,]



#-----------Install and load necessary packages if you haven't already
install.packages(c("caret", "ggplot2", "ggrepel", "olsrr"))
library(caret)
library(ggplot2)
library(ggrepel)
library(olsrr)


dev.off()

# Use olsrr for Cook's distance bar plot and other regression diagnostics
ols_plot_cooksd_bar(fwd_final1_3)

# Dffits plots
options(repr.plot.width = 12, repr.plot.height = 8)
plot(fwd_final1_3)
# dfbetas plots
ols_plot_dfbetas(fwd_final1_3)

# reject outliers
train_data_filtered <- train_data
train_data_filtered <- train_data_filtered[-125,]
train_data_filtered <- train_data_filtered[-48,]
train_data_filtered <- train_data_filtered[-47,]
train_data_filtered <- train_data_filtered[-42,]
train_data_filtered <- train_data_filtered[-41,]
train_data_filtered <- train_data_filtered[-40,]
train_data_filtered <- train_data_filtered[-11,]
train_data_filtered <- train_data_filtered[-10,]
View(train_data_filtered)

final_forward<- lm((price)~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear + `stroke:compressionratio`+stroke+compressionratio+`enginesize:compressionratio`, data = train_data_filtered)
summary(final_forward)

#------------------F test for entire model----------------------------------------------------------

summary(final_forward)
#F*= 240.5

#f value calculation
n<-nrow(train_data_filtered)
p<-10
f_value<-qf((1-0.05),p-1,n-p)
f_value

#f_value=1.94
#F*>f_value, not all bi is zero


#-----------------------------------T test for Beta values----------------------------------------

summary(final_forward)

tvalue<- qt(1-0.05/2,147)
tvalue

# stroke:compressionratio, stroke, compressionratio, enginesize:compressionratio should be removed as their t value is less than t(1-alpha/2, n-p)

# final model 0
final_forward0<- lm((price)~ curbweight+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = train_data_filtered)
summary(final_forward0)


# residual plots for model 0 ---------------------------------------
# linearity assumption
e_final_fwd0<- residuals(final_forward0)
plot(train_data_filtered$price,e_final_fwd)
#abline(lm(e_final_fwd~price,data=train_data_filtered))

#independence plot for model 0-------------

library(GGally)
library(ggplot2)

ggplot() +
  geom_point(aes(x = 1:length(e_final_fwd), y = e_final_fwd)) +
  geom_line(aes(x = 1:length(e_final_fwd), y = e_final_fwd), color = "blue") +  # Add lines
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Observation Order", y = "Residuals", title = "Residuals Independence Plot")



# qq plot
qqnorm(e_final_fwd)
qqline(e_final_fwd)

#normality test
MSE<-anova(final_forward0)["Residuals","Mean Sq"]
n<-nrow(train_data_filtered)
k<-rank(e_final_fwd)
p<-(k-0.375)/(n+0.25)
estimated_e<-qnorm(p)*MSE
rEe<-cor(estimated_e,e_final_fwd,method=c("pearson"))
rEe
# rEe < r_critical(0.987)


#bp test
library(lmtest)
bptest(final_forward0,studentize = FALSE)







# final model 1
final_forward1<- lm(1/(price)~ curbweight+enginesize+compressionratio+`enginesize:compressionratio`+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = train_data_filtered)
summary(final_forward1)
# residual plots for model 1 ---------------------------------------
# linearity assumption
e_final_fwd1<- residuals(final_forward1)
plot(train_data_filtered$price,e_final_fwd1)
#abline(lm(e_final_fwd~price,data=train_data_filtered))

#independence plot for model1

ggplot() +
  geom_point(aes(x = 1:length(e_final_fwd1), y = e_final_fwd1)) +
  geom_line(aes(x = 1:length(e_final_fwd1), y = e_final_fwd1), color = "blue") +  # Add lines
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Observation Order", y = "Residuals", title = "Residuals Independence Plot")

# qq plot
qqnorm(e_final_fwd)
qqline(e_final_fwd)

#normality test
MSE<-anova(final_forward1)["Residuals","Mean Sq"]
n<-nrow(train_data_filtered)
k<-rank(e_final_fwd)
p<-(k-0.375)/(n+0.25)
estimated_e<-qnorm(p)*MSE
rEe<-cor(estimated_e,e_final_fwd,method=c("pearson"))
rEe
#rEe>rcritical(0.987)

#bp test
library(lmtest)
bptest(final_forward1,studentize = FALSE)






# final model 2
final_forward2<- lm(log10(price)~ curbweight+compressionratio+`enginesize:compressionratio`+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = train_data_filtered)
summary(final_forward2)

# residual plots for model 2 ---------------------------------------
# linearity assumption
e_final_fwd2<- residuals(final_forward2)
plot(train_data_filtered$price,e_final_fwd)
#abline(lm(e_final_fwd~price,data=train_data_filtered))

#independence plot for model2

ggplot() +
  geom_point(aes(x = 1:length(e_final_fwd2), y = e_final_fwd2)) +
  geom_line(aes(x = 1:length(e_final_fwd2), y = e_final_fwd2), color = "blue") +  # Add lines
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Observation Order", y = "Residuals", title = "Residuals Independence Plot")

# qq plot
qqnorm(e_final_fwd)
qqline(e_final_fwd)

#normality test
MSE<-anova(final_forward2)["Residuals","Mean Sq"]
n<-nrow(train_data_filtered)
k<-rank(e_final_fwd)
p<-(k-0.375)/(n+0.25)
estimated_e<-qnorm(p)*MSE
rEe<-cor(estimated_e,e_final_fwd,method=c("pearson"))
rEe

#bp test
library(lmtest)
bptest(final_forward2,studentize = FALSE)




# ------------------ model validation ------------------
# model 1
# Model Validation
final_forward11<- lm(1/(price)~ curbweight+enginesize+compressionratio+`enginesize:compressionratio`+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = train_data_filtered)
summary(final_forward11)

final_forwardtest11<- lm(1/(price)~ curbweight+compressionratio+`enginesize:compressionratio`+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = test_data)
summary(final_forwardtest11)

# model 2
# Model Validation
final_forward22<- lm(log10(price)~ curbweight+compressionratio+`enginesize:compressionratio`+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = train_data_filtered)
summary(final_forward22)

final_forwardtest22<- lm(log10(price)~ curbweight+compressionratio+`enginesize:compressionratio`+enginesize+`enginesize:BrandType_Midrange`+BrandType_Midrange+enginelocation_rear, data = test_data)
summary(final_forwardtest22)
