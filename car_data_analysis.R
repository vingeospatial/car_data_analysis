# Load the libraries (do this every time you start)         
library(tidyverse)          # swiss army knife for data manipulation    
library(ggplot2)            # create beautiful charts and graphs    
library(corrplot)           # Makes correlation heatmaps
library(plotly)             # Interactive visualizations  
library(knitr)              # Pretty table formatting
library(DT)                 # Interactive data tables   

# print success message       
cat("All libraries loades successfully! Ready to explore data!\n")

# Load your dataset into R      
mtcars_data <-  read.csv("data/mtcars.csv")

# Let's take a first look at our data       
cat("Dataset loaded! Here's what we have:\n")     
print(paste("Number of cars:", nrow(mtcars_data)))         # Prints number of rows (cars)    
print(paste("Number of features:", ncol(mtcars_data)))     # Prints number of columns (feature)


# Display the first few rows (like previewing a book)      
head(mtcars_data)
colnames(mtcars_data)
str(mtcars_data)

# What columns do we have?      
cat("Column names in our dataset:\n")
colnames(mtcars_data)   # Prints all column names     

# what do the column mean? Let's create a data dictionary      
data_dictionary <- data.frame(
  column = c("model", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"),
  description = c("Car model name",
                  "Miles per gallon (fuel efficiency)",
                  "Number of cylinders",
                  "Engine displacement (cubic inches)",
                  "Horsepower",
                  "Rear axle ratio",
                  "Weight (1000 lbs)",
                  "Quarter mile time(seconds)",
                  "Engine shape (0=V-shaped, 1 = straight)",
                  "Transmission (0=automatic, 1=manual)",
                  "Number of gears",
                  "Number of carburetors")
                  
)

# Display our data dictionary      
knitr::kable(data_dictionary, caption = "What Each Column Means")    # Nicely formats and displays the dictionary as a table 


# CLEAN THE DATA FOR ANALYSIS     

# check for missing values (empty cells)       
cat("Checking for missing data:\n")
missing_data <- sum(is.na(mtcars_data))  # Count total missing values   
print(paste("Total missing values:", missing_data))   # Print the count

# Look at the structure of our data      
str(mtcars_data) # Shows data types and column structure    

#Convert categorical variables to factors (R's way of handling categories)

mtcars_data$vs <- factor(mtcars_data$vs, labels = c("v-shaped", "straight"))    # Engine shape as labels
mtcars_data$am <- factor(mtcars_data$am, labels = c("automatic", "manual"))     # Transmission type as labels   
mtcars_data$cyl <- factor(mtcars_data$cyl)   # Convert cylinder count to category
mtcars_data$gear <- factor(mtcars_data$gear) # Convert number of gears to 
mtcars_data$carb <- factor(mtcars_data$carb) # Convert carburetors to category

cat("Data cleaning complete! variables are properly formatted.\n")  # Confirmation message


# Explore our data - like being a detective!      

# Basic statistics summary     

cat("Basic Statistics Summary:\n")
summary(mtcars_data)  # Provides min, max, mean, and quartiles for each numeric column

# Let's look at fuel efficiency (mpg) - most important for car buyers !    
cat("\n Fuel Efficiency Analysis:\n")
print(paste("Most fuel-efficient car:", mtcars_data$model[which.max(mtcars_data$mpg)],
            "with", max(mtcars_data$mpg), "mpg"))  # Finds car with highest mpg   

print(paste("Least fuel-efficient car:", mtcars_data$model[which.min(mtcars_data$mpg)],
            "with", min(mtcars_data$mpg), "mpg"))  # Finds car with lowest mpg

# Let's see which cars have the most horsepower   
cat("\n Power Analysis:\n")
print(paste("Most powerful car:", mtcars_data$model[which.max(mtcars_data$hp)],
            "with", max(mtcars$hp), "horsepower"))  # Finds car with highest horsepower



# VISUALIZE THE DATA WITH CHARTS       

























