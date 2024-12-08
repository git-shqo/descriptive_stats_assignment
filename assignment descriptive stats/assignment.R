# Importing all required libraries (already installed)
library(gmodels)
library(ggplot2)
library(moments)
library(readxl)
library(corrplot)

# Importing the dataset
housing <- read_excel("housing.xlsx")
View(housing)

# Setting the seed as required
set.seed(0964409)
Houses <- housing[sample.int(546, 250, replace = FALSE),]

# 1. Creating a contingency table for the variables 'storeys' and 'bedrooms'

cross_tab <- CrossTable(x = Houses$storeys, # Storeys -> Rows of the table
                         y = Houses$bedrooms, # Bedrooms -> Columns of the table
                         prop.chisq = FALSE, # Don't include chi-square contribution of each cell
                         prop.r = FALSE, # Don't include row proportions
                         prop.c = FALSE, # Don't include column proportions
                         prop.t = FALSE, # Don't include table proportions
                        )

# 2.1. ECDF for bedrooms
ecdf_bedrooms <- ecdf(Houses$bedrooms) # Compute Empirical Cumulative
                                       # Distribution Function

plot(x = ecdf_bedrooms, # Data to be used for plotting
     main = "Cumulative Distribution of Bedrooms in the Houses of Windsor (Canada)", #Title
     xlab = "Number of Bedrooms", # X-axis
     ylab = "Cumulative Proportion of Houses", # Y-axis
     col = "blue", # Colour chosen
     pch = 20, # A graphical parameter indicating the used plotting symbol (circles in this case)
     cex = 1.5, # A point size (circles' size in this case)
     ) 

# 2.2. Quartiles
quartiles <- quantile(Houses$bedrooms, probs = c(0, 0.25, 0.5, 0.75, 1)) 
print(quartiles) # Print the quartiles

# 3.1. Drawing a density histogram for the variable "lotsize"

ggplot(data = Houses, aes(x = lotsize)) + # lotsize as X-axis
  geom_histogram(aes(y = ..density..), # density histogram 
                 color = "black", # Colour of the grids
                 fill = "darkblue", # Colour to be filled
                 bins = 16, # Number of bins
                 alpha = 0.8) + # Opacity rate of the histogram
  labs(
    title = "Distribution of Lot Sizes for Sampled Houses", # Title
    x = "Lot Size (square feet)", # X-axis
    y = "Density" #Y-axis
  ) + 
  theme_minimal() + # Background
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# 3.2 Calculating the skewness
skewness_lotsize <- skewness(x = Houses$lotsize) # Skewness
print(skewness_lotsize) # Print the skewness

# 3.3 Drawing an approximating gamma distribution

# Calculating the mean and variance of lotsize
mean_lotsize <- mean(Houses$lotsize) # Mean of lotsize
var_lotsize <- var(Houses$lotsize) # Variance of lotsize

# Estimate Gamma parameters
r <- mean_lotsize^2 / var_lotsize     # Shape parameter
lambda <- mean_lotsize / var_lotsize  # Rate parameters
sprintf("Shape parameter = %s, Rate parameter = %s", round(lambda, 4), round(r, 4)) # Display the parameters

# Create a Gamma density curve
x_vals <- seq(min(Houses$lotsize), # From
              max(Houses$lotsize), # To
              length.out = 250) # Length of the sequence

gamma_density <- dgamma(x_vals, # Inputs for gamma function
                        shape = r,
                        rate = lambda)

# Overlay Gamma PDF on histogram
ggplot(Houses, aes(x = lotsize)) + # Lotsize as X-axis
  geom_histogram(aes(y = ..density..), # Density histogram
                 color = "black", # Colour of the grids
                 fill = "darkblue", # Colour to be filled
                 bins = 16, # Number of bins
                 alpha = 0.8) + # Opacity rate of the histogram
  geom_line(aes(x = x_vals, # Domain for the gamma distribution
                y = gamma_density), # Range for the gamma distribution
            color = "red", # Colour of the graph
            size = 1.3) + # Thickness of the line
  labs(
    title = "Density of Lot Sizes with Fitted Gamma Distribution", # Title
    x = "Lot Size (square feet)", # X-axis
    y = "Density" #Y-axis
  ) + 
  theme_minimal() + # Background
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# 4. Creating boxplots
ggplot(Houses, aes(x = as.factor(garagepl), y = lotsize)) + #Data, garagepl as categorical variable, y
  geom_boxplot(fill = "cyan3", # Colour to be filled
               color = "black", # Colour of the grids
               outlier.color = "red") + # Colour of outliers
  labs(
    title = "Lot Size Distribution by Number of Garage Places", # Title
    x = "Number of Garage Places", # X-axis
    y = "Lot Size (square feet)" # Y-axis
  ) +
  theme_minimal() + # Background
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# 5. Finding the mean and s.d. to use coefficient of variation
pref_areas <- unique(Houses$prefarea) # Find the unique values of prefarea
mean1_conditioned <- mean(Houses[Houses$prefarea == pref_areas[1], "price"]$price) # mean1 cond. 
mean2_conditioned <- mean(Houses[Houses$prefarea == pref_areas[2], "price"]$price) # mean2 cond.
sd1_conditioned <- sd(Houses[Houses$prefarea == pref_areas[1], "price"]$price) # s.d.1 cond.
sd2_conditioned <- sd(Houses[Houses$prefarea == pref_areas[2], "price"]$price) # s.d.2 cond.
cv1 <- sd1_conditioned / mean1_conditioned # Coefficient of variation 1
cv2 <- sd2_conditioned / mean2_conditioned # Coefficient of variation 2

cat(cv1, cv2) # Print the coefficients

# 6. Calculating the correlation matrix
quantitative_var <- Houses[c(1,2,3,4,5,11)] # Choosing the quantitative variables
cor_quantitative_var = cor(quantitative_var) # Finding the correlation coefficients between variables
corrplot(corr = cor_quantitative_var, # Plotting the correlation matrix
         type = "full", # Type of correlation matrix
         method = "color", # The visualization method
         col.lim = c(0, 1), # The range for the colour-legend
         title = "Pairwise Correlations of Variables", # TItle
         addgrid.col = "black", # Grid colour
         tl.col = "black", # Colour of text label
         col = COL1("Blues"), # Colour gradient to be filled in elements of the matrix
         addCoef.col = "white", # Colour of the values 
         number.cex = 0.8, # Size of the values
         tl.srt = 45, # Rotate the variables by 45 degress
         mar = c(0, 0, 2, 0), # Adding some space for the title
         number.digits = 4) # Number of digits


# 7. Drawing a scatter plot
Houses$log_lotsize <- log(Houses$lotsize) # Natural log of lotsize
Houses$log_price <- log(Houses$price) # Natural log of price
model <- lm(log_price ~ log_lotsize, data = Houses) # Regresssion line
residuals <- resid(model) # Residuals

ggplot(Houses, aes(x=log_lotsize, y=log_price)) + # Lotsize as X, Price as Y
  geom_point() + # Create a scatterplot
  geom_smooth(method=lm, # Draw a regression line
              color="red", # Colour of the line
              se=FALSE) + # Don't display confidence interval
  labs(
    x = "Lotsize", # X-axis
    y = "Price", # Y-axis
    title = "Relationship Between Lot Size and House Price" 
  ) + theme_minimal() + # Background
  theme(plot.title = element_text(hjust = 0.5)) # Center the title


qqnorm(residuals, # Normal-quantile plot
       main = "Normal Q-Q Plot of Residuals", # title
       col = "blue")  # colour
qqline(residuals, col = "red", lwd = 2) # Reference line