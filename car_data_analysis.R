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

# Chart 1: Fuel efficiency distribution

ggplot(mtcars_data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) + # Histogram of MPG
  labs(title = "Distribution of fuel efficiency (MPG)",  # Main chart title
       subtitle = "how fuel efficient are this cars?",   # subheading
       x = "miles per galon(MPG)", #x-xis label
       y = "Number of cars"        #y-axis label
       ) +
  theme_minimal() +  #clean theme
  theme(plot.title = element_text(size = 16, face = "bold")) # Bold title styling


# Chart 2: Horsepower vs Fuel efficiency  
ggplot(mtcars_data, aes(x = hp, y = mpg)) +
  geom_point(size = 3, alpha = 0.7, color = "red") +             # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +       # Linear trend line
  labs(title = "Power vs Efficiency: The Trade-off",           # Chart title
       subtitle = "Do more powerful cars use more fuel?",        # Subheading
       x = "Horsepower",                                         # X-axis label
       y = "Miles per Gallon (MPG)") +                           # Y-axis label
  theme_minimal()                                                # Clean look



# Chart 3: Transmission type comparison
ggplot(mtcars_data, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot(alpha = 0.7) +                                      # Box plot by transmission type
  labs(title = "🔧 Manual vs Automatic: Fuel Efficiency Battle",   # Chart title
       x = "Transmission Type",                                    # x-axis label
       y = "Miles per Gallon (MPG)",                               # y-axis label
       fill = "Transmission") +                                    # legend label
  theme_minimal() +                                                # clean theme
  scale_fill_manual(values = c("orange", "green"))                 # custom colors for boxes



# DISCOVER RELATIONSHIPS BETWEEN VARIABLES   
# Step 8: Find relationships between variables – like finding patterns!

  
# select only numeric columns for correlation 
numeric_data <- mtcars_data |> 
  select_if(is.numeric) |>      # keep only numeric columns
  select(-matches("model"))     # Remove the model column if it's present



# Create correlation matrix
correlation_matrix <- cor(numeric_data)   # Compute pairwise correlations

# Visualize correlations with a heatmap
corrplot(correlation_matrix,
         method = "color",                # Use color blocks to show strength
         type = "upper",                  # Show only the upper triangle
         order = "hclust",                # Cluster similar variables together
         tl.cex = 0.8,                    # Size of variable labels
         tl.col = "black",                # Label color
         title = "Correlation Heatmap: How Variables Relate")  # Chart title

# Find strongest correlations
cat("🔗 Strongest Relationships:\n")

# Convert correlation matrix to find top correlations
cor_pairs <- which(abs(correlation_matrix) > 0.7 & correlation_matrix != 1, arr.ind = TRUE)  # Filter strong correlations

# Loop through and print variable pairs with high correlation
for(i in 1:nrow(cor_pairs)) {
  row_var <- rownames(correlation_matrix)[cor_pairs[i,1]]
  col_var <- colnames(correlation_matrix)[cor_pairs[i,2]]
  cor_value <- round(correlation_matrix[cor_pairs[i,1], cor_pairs[i,2]], 3)
  print(paste(row_var, "and", col_var, "correlation:", cor_value))  # Print each strong correlation pair
}


# Create correlation matrix      
correlation_matrix <- cor(numeric_data)   # compute pairwise correlations

# visualize correlation with a heatmap   
corrplot(correlation_matrix,
         method = "color",                                      # use color blocks to show strength
         type = "upper",                                        # Show only the upper triangle
         order = "hclust",                                      # cluster similar variables together
         tl.cex = 0.8,                                          # size of variable labels
         tl.col = "black",                                      # Label color
         title = "Correlation Heatmap ; How Variables Relate")  # Chart title


cat("🔗 Strongest Relationships:\n")  

cor_df <- as.data.frame(as.table(correlation_matrix))

# Filter strong correlations (>|0.7| but not self-correlations)
cor_df <- cor_df[abs(cor_df$Freq) > 0.7 & cor_df$Freq != 1, ]

# Remove duplicate pairs (A-B same as B-A)
cor_df <- cor_df[!duplicated(t(apply(cor_df[, 1:2], 1, sort))), ]

# Sort by absolute correlation value (ascending)
cor_df <- cor_df[order(abs(cor_df$Freq),decreasing = TRUE),]


# Print results
for (i in seq_len(nrow(cor_df))) {
  row_var <- cor_df[i, 1]
  col_var <- cor_df[i, 2]
  cor_value <- round(cor_df[i, 3], 3)
  print(paste(row_var, "and", col_var, "correlation:", cor_value))
}





# cat("🔗 Strongest Relationships:\n")  
# 
# # Convert correlation matrix to find the top correlation   
# cor_pairs <- which(abs(correlation_matrix) > 0.7 & correlation_matrix !=1, arr.ind = TRUE)   # Filter strong correlations
# 
# # Loop through and print variable pairs with high correlation    
# for(i in 1:nrow(cor_pairs)) {
#   row_var <- rownames(correlation_matrix[cor_pairs[i,1]])
#   cor_var <- colnames(correlation_matrix)[cor_pairs[i,2]]
#   cor_value <- round(correlation_matrix[cor_pairs[i, 1], cor_pairs[i, 2]], 3)
#   print(paste(row_var, "and", col_var, "correlation:", cor_value))     # Print ech strong correlation pair
# }


# PERFORM DEEPER GROUP ANALYSIS     

# Step 9: Dig deeper – advanced insights!

# Group analysis by number of cylinders
cylinder_analysis <- mtcars_data |> 
  group_by(cyl) |>                                  # Group data by cylinder count
  summarise(
    count = n(),                                    # Number of cars in each group
    avg_mpg = round(mean(mpg), 2),                  # Average miles per gallon
    avg_hp = round(mean(hp), 2),                    # Average horsepower
    avg_weight = round(mean(wt), 2),                # Average weight
    .groups = 'drop'                                # Drop grouping structure
  )

# Display summary table
cat("Analysis by Number of Cylinders:\n")
knitr::kable(cylinder_analysis, caption = "Performance by Engine Size")  # Nicely format the table

# Create a comprehensive comparison chart
ggplot(mtcars_data, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_violin(alpha = 0.7) +                        # Violin plot shows MPG distribution shape
  geom_boxplot(width = 0.2, alpha = 0.8) +          # Boxplot adds summary stats (median, quartiles)
  labs(title = "Fuel Efficiency by Engine Size", # Chart title
       subtitle = "Distribution of MPG across different cylinder counts",  # Subheading
       x = "Number of Cylinders",                   # X-axis label
       y = "Miles per Gallon (MPG)") +              # Y-axis label
  theme_minimal()                                   # Clean layout



# PREDICT FUEL EFFICIENCY WITH A SIMPLE MODEL    


# step 10: predict fuel efficiency with a simple model    

# Create a simple linear model to predict MPG based on weight and horsepower
model <- lm(mpg ~ wt + hp, data = mtcars_data)      # Fit a linear regression model

# Model summary
cat("Fuel Efficiency Prediction Model:\n")
summary(model)                                      # View coefficients, p-values, R², etc.

# Make predictions for our existing cars
mtcars_data$predicted_mpg <- predict(model)         # Add predicted MPG as a new column

# Compare actual vs predicted
comparison <- mtcars_data |> 
  select(model, mpg, predicted_mpg) |>              # Keep only relevant columns
  mutate(
    predicted_mpg = round(predicted_mpg, 2),        # Round predictions for readability
    difference = round(mpg - predicted_mpg, 2)      # Positive means model underestimated MPG
  )

cat("\n Actual vs Predicted MPG (first 10 cars):\n")
head(comparison, 20) |>  knitr::kable()             # Display first 10 comparisons as a neat table

























