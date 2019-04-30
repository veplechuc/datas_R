#set directory
setwd("C:/dev/repos/R/datas_r")

#read file
cars <- read.table(file = "cars.txt", header = TRUE, sep = "\t", quote = "\"")

#look
head(cars)

#using dplyr package
#library(dplyr)


# Select a subset of columns
temp <- select(
  .data = cars, 
  Transmission,
  Cylinders,
  Fuel.Economy)


# Inspect the results
head(temp)

# Filter a subset of rows
temp <- filter(
  .data = temp, 
  Transmission == "Automatic")

# Inspect the results
head(temp)

# Compute a new column
#create a new column that represent the consumption of km/l
temp <- mutate(
  .data = temp, 
  Consumption = Fuel.Economy * 0.425)

# Inspect the results
head(temp)

# Group by a column
#returns a gruoup by data frame instead
temp <- group_by(
  .data = temp,  
  Cylinders)

# Inspect the results
head(temp)

# Aggregate based on groups
temp <- summarize(
  .data = temp, 
  Avg.Consumption = mean(Consumption))

# Inspect the results
head(temp)

# Arrange the rows in descending order
temp <- arrange(
  .data = temp, 
  desc(Avg.Consumption))

# Inspect the results
head(temp)

# Convert to data frame
efficiency <- as.data.frame(temp)

# Inspect the results
print(efficiency)

# Chain methods together
efficiency <- cars %>%
  select(Fuel.Economy, Cylinders, Transmission) %>%
  filter(Transmission == "Automatic") %>%
  mutate(Consumption = Fuel.Economy * 0.425) %>%
  group_by(Cylinders) %>%
  summarize(Avg.Consumption = mean(Consumption)) %>%
  arrange(desc(Avg.Consumption)) %>%
  as.data.frame()

# Inspect the results
print(efficiency)

# Save the results to a CSV file
write.csv(
  x = efficiency,
  file = "Fuel Efficiency.csv",
  row.names = FALSE)




#############################################################
# Creating Descriptive Statistics

# Set the working directory
setwd("C:/dev/repos/R/datas_r")

# Read a CSV data file
cars <- read.csv("Cars.csv")

# Peek at the data
head(cars)

# Create a frequency table - look for number of occurrencies
table(cars$Transmission)

# Get the minimum value
min(cars$Fuel.Economy)

# Get the maximum value
max(cars$Fuel.Economy)

# Get the average value
mean(cars$Fuel.Economy)

# Get the median value
median(cars$Fuel.Economy)

# Get the quartiles
quantile(cars$Fuel.Economy)

# Get the standard deviation
sd(cars$Fuel.Economy)

# Get the total value
sum(cars$Fuel.Economy)

# Get the correlation coefficient
cor(
  x = cars$Cylinders,
  y = cars$Fuel.Economy)

# Summarize an entire table
summary(cars)

########################################################
# Creating Data Visualization


# Read the CSV file
cars <- read.csv("Cars.csv")

# Load the ggplot2 library
library(ggplot2)

# Create a frequency bar chart
ggplot(
  data = cars, 
  aes(x = Transmission)) + 
  geom_bar() +
  ggtitle("Count of Cars by Transmission Type") +
  xlab("Transmission Type") +
  ylab("Count of Cars")

# Create a histogram
ggplot(
  data = cars, 
  aes(x = Fuel.Economy)) +
  geom_histogram(
    bins = 100) +
  ggtitle("Distribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Count of Cars")

# Create a density plot
ggplot(
  data = cars, 
  aes(x = Fuel.Economy)) +
  geom_density() +
  ggtitle("Distribution of Fuel Economy") +
  xlab("Fuel Economy (mpg)") +
  ylab("Density")

# Create a scatterplot
ggplot(
  data = cars, 
  aes(
    x = Cylinders,
    y = Fuel.Economy)) +
  geom_point() +
  ggtitle("Fuel Economy by Cylinders") +
  xlab("Number of Cylinders") +
  ylab("Fuel Economy (mpg)")


###########################################################################
# Creating Statistical Models

# Load the data - it is pre intalled with R
data(iris)

# Peak at data
head(iris)

# Create a scatterplot
plot(
  x = iris$Petal.Length, 
  y = iris$Petal.Width,
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Create a linear regression model
model <- lm(
  formula = Petal.Width ~ Petal.Length,
  data = iris)

# Summarize the model
summary(model)

# Draw a regression line on plot
lines(
  x = iris$Petal.Length,
  y = model$fitted, 
  col = "red",
  lwd = 3)

# Get correlation coefficient
cor(
  x = iris$Petal.Length,
  y = iris$Petal.Width)

# Predict new values from the model
predict(
  object = model, 
  newdata = data.frame(
    Petal.Length = c(2, 5, 7)))
#############################################################################
# Handling Big Data

# Set working directory
setwd("C:/dev/repos/R/datas_r")

# Load the ff package
library(ff)

# Read a CSV file as ff data frame
irisff <- read.table.ffdf(
  file = "Iris.csv",
  FUN = "read.csv")

# Inspect the class
class(irisff)

# Inspect the column names
names(irisff)

# Inspect the first few rows
irisff[1:5,]

# Load the biglm package
library(biglm)

# Create a linear regression model
model <- biglm(
  formula = Petal.Width ~ Petal.Length,
  data = irisff)

# Summarize the model
summary(model)

# Create a scatterplot
plot(
  x = irisff$Petal.Length[], 
  y = irisff$Petal.Width[],
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Get y-intercept from model
b <- summary(model)$mat[1,1]

# Get slope from model
m <- summary(model)$mat[2,1]

# Draw a regression line on plot
lines(
  x = irisff$Petal.Length[],
  y = m * irisff$Petal.Length[] + b, 
  col = "red",
  lwd = 3)

# Predict new values with the model
predict(
  object = model,
  newdata = data.frame(
    Petal.Length = c(2, 5, 7),
    Petal.Width = c(0, 0, 0)))


######################################################################
# Predicting with Machine Learning

# Load the data
data(iris)

# Set a seed to make randomness reproducible
set.seed(42)

# Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:150, 
  size = 100)

# Inspect the random indexes
print(indexes)

# Create a training set from indexes
train <- iris[indexes, ]

# Create a test set from remaining indexes
test <- iris[-indexes, ]

# Load the decision tree package
library(tree)

# Train a decision tree model
model <- tree(
  formula = Species ~ .,
  data = train)

# Inspect the model
summary(model)

# Visualize the decision tree model
plot(model)
text(model)

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

# Create a scatterplot colored by species
plot(
  x = iris$Petal.Length, 
  y = iris$Petal.Width,
  pch = 19,
  col = palette[as.numeric(iris$Species)],
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Plot the decision boundaries
partition.tree(
  tree = model,
  label = "Species",
  add = TRUE)

# Predict with the model
predictions <- predict(
  object = model,
  newdata = test,
  type = "class")

# Create a confusion matrix
table(
  x = predictions, 
  y = test$Species)

# Load the caret package
library(caret)

# Evaluate the prediction results
confusionMatrix(
  data = predictions, 
  reference = test$Species)

# Set working directory
setwd("C:/dev/repos/R/datas_r")

# Save the tree model
save(model, file = "Tree.RData")

# Save the training data
save(train, file = "Train.RData")


#############################################################################
# Deploying to Production

# Load shiny
library(shiny)

# Create a UI
ui <- fluidPage("Hello World!")

# Create a server
server <- function(input, output) {}

# Create a shiny app
shinyApp(
  ui = ui,
  server = server)

# Create a UI with I/O controls
ui <- fluidPage(
  titlePanel("Input and Output"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "num",
        label = "Choose a Number",
        min = 0,
        max = 100,
        value = 25)),
    mainPanel(
      textOutput(
        outputId = "text"))))

# Create a server than maps input to output
server <- function(input, output) {
  output$text <- renderText({
    paste("You selected ", input$num )})
}

# Create a shiny app
shinyApp(
  ui = ui,
  server = server)

# Load decision tree package
library(tree)

# Set working directory
setwd("C:/Pluralsight")

# Load training data
load("Train.RData")

# Load tree model
load("Tree.RData")

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

# Create user interface code
ui <- fluidPage(
  titlePanel("Iris Species Predictor"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "petal.length",
        label = "Petal Length (cm)",
        min = 1,
        max = 7,
        value = 4),
      sliderInput(
        inputId = "petal.width",
        label = "Petal Width (cm)",
        min = 0.0,
        max = 2.5,
        step = 0.5,
        value = 1.5)),
    mainPanel(
      textOutput(
        outputId = "text"),
      plotOutput(
        outputId = "plot"))))

# Create server code
server <- function(input, output) {
  output$text = renderText({
    
    # Create predictors
    predictors <- data.frame(
      Petal.Length = input$petal.length,
      Petal.Width = input$petal.width,
      Sepal.Length = 0,
      Sepal.Width = 0)
    
    # Make prediction
    prediction = predict(
      object = model,
      newdata = predictors,
      type = "class")
    
    # Create prediction text
    paste(
      "The predicted species is ",
      as.character(prediction))
  })
  
  output$plot = renderPlot({
    
    # Create a scatterplot colored by species
    plot(
      x = iris$Petal.Length, 
      y = iris$Petal.Width,
      pch = 19,
      col = palette[as.numeric(iris$Species)],
      main = "Iris Petal Length vs. Width",
      xlab = "Petal Length (cm)",
      ylab = "Petal Width (cm)")
    
    # Plot the decision boundaries
    partition.tree(
      model,
      label = "Species",
      add = TRUE)
    
    # Draw predictor on plot
    points(
      x = input$petal.length,
      y = input$petal.width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2)
  })
}

# Create a shiny app
shinyApp(
  ui = ui,
  server = server)


