# Linear Regression Assignment by Gaurav Sachdeva

library(plyr)
library(dplyr)
library(tidyr)
library(car)
library(MASS)
library(Hmisc)
library(stringr)
library(ggplot2)

#setting working directory for reading the file

setwd("C:/PGDDS/Linear Regression assignment")

# Reading the data file
geelycars_df <- read.csv("CarPrice_Assignment.csv")

#Understanding the structure and summary
str(geelycars_df)
summary(geelycars_df)

# Data claening ad prepration

#Checking for NA values.No NA values found
n=sum(is.na(geelycars_df))
if(n==0) {print("No NA values")} else {print("NA values.Need Cleaning")}

#Checking for duplicated values.No duplicated values found
which(duplicated(geelycars_df))

#Rechecking for all variables
describe(geelycars_df)

#Now converting categorical variables with two levels into numeric

# for fuel type diesel (0), gas(1)
table(factor(geelycars_df$fueltype))#checking for two levels
levels(geelycars_df$fueltype) <- c(0,1) 
geelycars_df$fueltype <- as.numeric(levels(geelycars_df$fueltype))[geelycars_df$fueltype]


#for aspiration std (0), turbo(1)
table(factor(geelycars_df$aspiration))
levels(geelycars_df$aspiration) <- c(0,1) 
geelycars_df$aspiration <- as.numeric(levels(geelycars_df$aspiration))[geelycars_df$aspiration]

#for door number four (0), two (1)
table(factor(geelycars_df$doornumber))
levels(geelycars_df$doornumber) <- c(0,1) 
geelycars_df$doornumber <- as.numeric(levels(geelycars_df$doornumber))[geelycars_df$doornumber]

#for enginelocation front (0), rear(1)
table(factor(geelycars_df$enginelocation))
levels(geelycars_df$enginelocation) <- c(0,1) 
geelycars_df$enginelocation <- as.numeric(levels(geelycars_df$enginelocation))[geelycars_df$enginelocation]

# Treating the CarName column to separate CarCompany and Car Model

geelycars_df$CarName <-trimws(tolower(geelycars_df$CarName))
Car_Model<- str_split_fixed(geelycars_df$CarName, "[ ]", 2)
str(Car_Model)
Car_Model_df<-as.data.frame(Car_Model)

geelycars_df$CarName=Car_Model_df$V1

#Discarding the model as it is not significant in analaysis

#geelycars_df$Car_Model = rep(1,nrow(geelycars_df))
#geelycars_df$Car_Model=Car_Model_df$V2


levels(as.factor(geelycars_df$CarName))

# Treating the speeling mistakes in he CarName column

geelycars_df$CarName <- mapvalues(geelycars_df$CarName, from = c("maxda", "porcshce", "vokswagen", "vw",  "toyouta"), to = c("mazda", 
                                                                                     "porsche", "volkswagen", "volkswagen", "toyota"))
#Doing Some Outlier treatment
boxplot(geelycars_df$price)
firstquad1 <- quantile(geelycars_df$price,0.25)
thirdquad1<-quantile(geelycars_df$price,0.75)
upperlmtforout1<-quantile(geelycars_df$price,0.75)+1.5*(thirdquad1-firstquad1)
lowerlmtforout1<-quantile(geelycars_df$price,0.25)-1.5*(thirdquad1-firstquad1) # since it is negative npt conisdered
geelycars_df<-subset(geelycars_df,geelycars_df$price<=upperlmtforout1) # Ignoring values above 99 percent quantile


# multilevel variables are converted to dummy and then to numbers

#1. companyname
d_carname <- data.frame(model.matrix( ~CarName, data =geelycars_df))

#2. carbody

d_carbody <- data.frame(model.matrix( ~carbody, data = geelycars_df))

#3. drivewheel

d_drivewheel <- data.frame(model.matrix( ~drivewheel, data = geelycars_df))

#4. cylindernumber
d_cylnum <- data.frame(model.matrix( ~cylindernumber, data = geelycars_df))

#5. enginetype
d_enginetype <- data.frame(model.matrix( ~enginetype, data = geelycars_df))

#6. fuelsystem
d_fuelsys <- data.frame(model.matrix( ~fuelsystem, data = geelycars_df))


# Inserting dummy variables created earlier for analysis

#Changing dataset before insertion
geelycars_df_new<-geelycars_df

# Inserting dummy var for carbody
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-carbody),d_carbody[,-1])

# Inserting dummy var for drivewheel
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-drivewheel),d_drivewheel[,-1])

# Inserting dummy var for enginetype
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-enginetype),d_enginetype[,-1])

# Inserting dummy var for cylinder number
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-cylindernumber),d_cylnum[,-1])

# Inserting dummy var for fuelsystem
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-fuelsystem),d_fuelsys[,-1])

# Inserting dummy var for CarName
geelycars_df_new<-cbind(geelycars_df_new %>% dplyr::select(-CarName),d_carname[,-1])

#Inserting some derived matrices which may be of some value

#1. Avg mpg
geelycars_df_new$avgmpg <- round((geelycars_df_new$citympg + geelycars_df_new$highwaympg)/2,1)

#2. mpg to HP ratio
geelycars_df_new$mpg2hp <- round(geelycars_df_new$avgmpg/geelycars_df_new$horsepower,1)

#3. mpg to curbweight ratio 
geelycars_df_new$mpg2cw <-round(geelycars_df_new$avgmpg/geelycars_df_new$curbweight,2)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(geelycars_df_new), 0.7*nrow(geelycars_df_new))
# generate the train data set
train = geelycars_df_new[trainindices,]

#Similarly store the rest of the observations into an object "test".
test =geelycars_df_new[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#Now use stepAIC to throw away variables 

step <- stepAIC(model_1, direction="both")

# All other variables except the below need to be considered

model_2 <- lm(price ~ car_ID + aspiration + carlength + curbweight + boreratio + 
                citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + drivewheelrwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspfi + CarNameaudi + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + mpg2hp, data = train)

summary(model_2)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_2)

# Dropping car_ID as per business knowledge and other values on basis of their p-values(>0.05) i.e statistically insignificant

model_3 <- lm(price ~ aspiration + carlength + curbweight + 
                citympg + carbodyhardtop + carbodyhatchback +  
                carbodywagon + drivewheelfwd + drivewheelrwd + enginetypel + 
                enginetypeohcf + fuelsystem2bbl + CarNameaudi + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + mpg2hp, data = train)

summary(model_3)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_3)

# Dropping variables on basis of their p-values(>0.05) i.e statistically insignificant

model_4 <- lm(price ~ aspiration + carlength + curbweight + 
                 carbodyhatchback +  
                carbodywagon + drivewheelfwd + drivewheelrwd + enginetypel + 
                 CarNameaudi + CarNamebmw + CarNamebuick + 
               + CarNamemitsubishi +  CarNameplymouth + 
                CarNameporsche  , data = train)
summary(model_4)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_4)

# Dropping variables on basis of their p-values(>0.05) i.e statistically insignificant

model_5 <- lm(price ~ aspiration + curbweight + 
                carbodywagon + drivewheelfwd + drivewheelrwd + enginetypel + 
                CarNameaudi + CarNamebmw + CarNamebuick + 
                + CarNamemitsubishi + CarNameporsche  , data = train)
summary(model_5)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_5)

# Dropping variable with single * and VIF>2

model_6 <- lm(price ~ aspiration + curbweight + 
                carbodywagon +  drivewheelrwd + enginetypel + 
                CarNameaudi + CarNamebmw + CarNamebuick + 
                + CarNamemitsubishi + CarNameporsche  , data = train)
summary(model_6)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_6)

# Dropping variable with VIF>2

model_7 <- lm(price ~ aspiration + curbweight + 
                carbodywagon +  enginetypel + 
                CarNameaudi + CarNamebmw + CarNamebuick + 
                + CarNamemitsubishi + CarNameporsche  , data = train)
summary(model_7)

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_7)

# Dropping variable aspiration as it seems weakest with two*

model_8 <- lm(price ~ curbweight + 
                carbodywagon +  enginetypel + 
                CarNameaudi + CarNamebmw + CarNamebuick + 
                + CarNamemitsubishi + CarNameporsche , data = train)
summary(model_8)


## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_8)

# Dropping variable CarNamemitsubishi as it seems weakest with single *

model_9 <- lm(price ~ curbweight + 
                carbodywagon +  enginetypel + 
                CarNameaudi + CarNamebmw + CarNamebuick + 
                 + CarNameporsche , data = train)
summary(model_9)
#Multiple R-squared:  0.8589,	Adjusted R-squared:  0.851

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_9)

# Dropping variable CarNameaudi as it seems weakest 

model_10 <- lm(price ~ curbweight + 
                 carbodywagon +  enginetypel + 
                 CarNamebmw + CarNamebuick + 
                 + CarNameporsche , data = train)
summary(model_10)
#All variables with 3 stars are achieved
#Multiple R-squared:  0.8495,	Adjusted R-squared:  0.8424 

## checking multicollinearity (VIF>2 to be dropped if statistically insignificant)
vif(model_10)

# predicting the results in test dataset
Predict_1 <- predict(model_10,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
View(rsquared)