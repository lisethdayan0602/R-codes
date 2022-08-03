
install.packages("Hmisc")
library(Hmisc)

install.packages("ggm")
library(ggm)

install.packages("polycor")
library(polycor)

#  Download the Caret package
install.packages("caret")
library(caret)

# Access dataset cars
data(cars)
force(cars)
View(cars)


# Correlations between Price, Mileage, and Cylinder
cor(cars[ , c('Price', 'Mileage', 'Cylinder')],
    use='complete')


# Regression predicting price from mileage.
carsPrice <- lm(cars$Price ~ cars$Mileage,data = cars)
carsPrice <- lm(Price ~ Mileage)
summary(carsPrice)

# Create the regression equation
carsPrice$coefficients


cars$Price[1]

cars$Mileage[1]

carsPrice$residuals[1]


b0 = carsPrice$coefficients[1] 
b1 = carsPrice$coefficients[2]
cars$Mileage = cars$Mileage[1] 
e1 = carsPrice$residuals[1]
print(b0 + b1*cars$Mileage + e1) ## This result is giving me the correct value,
                                 ## but it is printing it more than once!


# Regression predicting price from mileage and cylinde
carsPrice <- lm(cars$Price ~ cars$Mileage + cars$Cylinder, data = cars)
carsPrice <- lm(Price ~ Mileage+Cylinder)
summary(carsPrice)

# Create the regression equation

# Coefficients
carsPrice$coefficients

cars$Price[1]

cars$Mileage[1]

cars$Cylinder[1]

# Reciduals
carsPrice$residuals[1]

# Equation
b0 = carsPrice$coefficients[1] 
b1 = carsPrice$coefficients[2]
b2 = carsPrice$coefficients[3]
cars$Mileage = cars$Mileage[1]
cars$Cylinder = cars$Cylinder[1]
e1 = carsPrice$residuals[1]
print(b0 + b1*cars$Mileage + b2*cars$Cylinder + e1) # I need to work in solving this

# standardized regression coefficients
lm.beta(cars) 


                         
# Get data on Sacramento's housing market
data(Sacramento)
force(Sacramento)
View(Sacramento)

# Correlations between beds, baths, square feet, and price
cor(Sacramento[ , c('beds', 'baths', 'sqft', 'price')],
    use='complete')

# Regression predicting price from beds
Sacramentoprice <- lm(Sacramento$price ~ Sacramento$beds,data = Sacramento)
Sacramentoprice <- lm(price ~ beds)
summary(Sacramentoprice)

# Adding sqft as a predictor
Sacramentoprice <- lm(Sacramento$price ~ Sacramento$beds + Sacramento$sqft, data = Sacramento)
Sacramentoprice <- lm(price ~ beds+sqft)
summary(Sacramentoprice)

# Regression predicting price from type
Sacramentoprice <- lm(Sacramento$price ~ Sacramento$type,data = Sacramento)
Sacramentoprice <- lm(price ~ type)
summary(Sacramentoprice)




























