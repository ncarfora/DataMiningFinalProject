library(tidymodels)
library(tidyverse)
library(baguette)
library(vip)
library(pdp)
library(kernlab)
library(readr)
library(ggplot2)
customer_retention <- read.csv("customer_retention.csv")
head(customer_retention)
str(customer_retention)
summary(customer_retention)

# DATA CLEANING
# Finding missing values--11 in Total Charges column
colSums(is.na(customer_retention))


# Omiited rows with missing values
customer_retention <- na.omit(customer_retention)

# Ensured Consistency in Categorical Variables
unique(customer_retention$Status)
unique(customer_retention$Gender)
unique(customer_retention$SeniorCitizen)
unique(customer_retention$Partner)
unique(customer_retention$Dependents)
unique(customer_retention$PhoneService)
unique(customer_retention$MultipleLines)
unique(customer_retention$InternetService)
unique(customer_retention$OnlineSecurity)
unique(customer_retention$OnlineBackup)
unique(customer_retention$DeviceProtection)
unique(customer_retention$TechSupport)
unique(customer_retention$StreamingTV)
unique(customer_retention$StreamingMovies)
unique(customer_retention$Contract)
unique(customer_retention$PaperlessBilling)



# Exploring Predictor Variables (continuous)

# Tenure (Number of Months customers have stayed w the company)
cust_tenure <- ggplot(customer_retention, aes(x = Tenure)) +
  geom_histogram(binwidth = 5, fill ="blue") + 
  theme_grey() +
  scale_x_continuous(n.breaks=10)+
  labs(title = "Customer Tenure", x = "Number of Months Kept", y = "Count of Instances")
cust_tenure

# Monthly Charges
monthly_charges<- ggplot(customer_retention, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 3, fill = "darkred") +
  theme_grey() +
  scale_x_continuous(n.breaks=10)+
  labs(title = "Customer Monthly Charges", x = "Amount Billed ($)", y = "Count of Instances")
monthly_charges

# Total Charges
total_charges <- ggplot(customer_retention, aes(x = TotalCharges))+
  geom_histogram(binwidth = 1000, fill = "darkgreen")+
  theme_grey()+
  scale_x_continuous(n.breaks=10)+
  labs(title="Total Customer Charges", y = "Count of Instances", x = "Total Charged ($)")
total_charges


# Exploring Categorical Variables
# Internet Service Usage - Fiber Optic Leads
internet_service <- ggplot(customer_retention, aes(x = InternetService)) + 
  geom_bar(fill = "blue") +
  theme_minimal()
internet_service

# Gender - Fairly Even
gender <- ggplot(customer_retention, aes(x = Gender)) + 
  geom_bar(fill = "green") +
  theme_minimal()
gender

# Senior Citizen - Mostly NO
senior_citizen <- ggplot(customer_retention, aes(x = SeniorCitizen)) + 
  geom_bar(fill = "blue") +
  theme_minimal()
senior_citizen

# Relationships
# Tenure vs Service
tenure_service <- ggplot(customer_retention, aes(x = InternetService, y = Tenure)) +
  geom_boxplot(fill = "orange") +
  theme_minimal()
tenure_service

# Demographics and Service
senior_service <- ggplot(customer_retention, aes(x = SeniorCitizen, fill = InternetService)) +
  geom_bar(position = "dodge") +
  theme_minimal()
senior_service


# Variable Response Rate
# Baseline Churn Rate
table(customer_retention$Status)
prop.table(table(customer_retention$Status))

# Tenure vs Status
# this is kind of a given
tenure_status <- ggplot(customer_retention, aes(x = Status, y = Tenure)) +
  geom_boxplot(fill = "purple") +
  theme_minimal()
tenure_status

# Monthly Charge vs Status 
monthly_status <- ggplot(customer_retention, aes(x = Status, y = MonthlyCharges)) +
  geom_boxplot(fill = "red") +
  theme_minimal()
monthly_status





