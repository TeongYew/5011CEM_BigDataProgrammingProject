
#data cleaning
library(janitor)
df2 <- clean_names(df)
df3 <- remove_empty(df2, which = c("rows", "cols"), quiet = FALSE)
library(dplyr)
df_cleaned <- distinct(df3)


#sequential vs parallel processing

library(doParallel)
library(microbenchmark)
library(tidyverse)

load_csv_function <- function(){
  df <-
    list.files(pattern = "*.csv") %>%
    map_df(~read_csv(.))
  df
}

numCores <- detectCores()
numCores
registerDoParallel(cores = numCores)

load_csv_function()

library(plyr)
mbm <- microbenchmark(sequential = llply(load_csv_function()),
                      parallel = llply(load_csv_function(), .parallel = TRUE))

mbm

llply(sequential(), .parallel = TRUE)


library(ggplot2)
autoplot(mbm)


#loading the files

library(tidyverse)

df <-
  list.files(pattern = "*.csv") %>%
  map_df(~read_csv(.))
df
view(df)

obesity_data <- read.csv("london_obesity_borough_2012.csv", header=TRUE, sep=",")
view(obesity_data)

grocery_data <- read.csv("year_borough_grocery_cleaned.xlsb.csv", header=TRUE, sep=",")
view(grocery_data)


#rename one of the column names for merging later
colnames(obesity_data)[colnames(obesity_data) == "oslaua"] <- "area_id"

#merge the files using the area_id
combined_data <- merge(obesity_data, grocery_data, by="area_id")
view(combined_data)

#descriptive statistics
summary(combined_data$f_obese)
IQR(combined_data$f_obese)
sd(combined_data$f_obese)
var(combined_data$f_obese)

summary(combined_data$f_energy_fibre)
IQR(combined_data$f_energy_fibre)
sd(combined_data$f_energy_fibre)
var(combined_data$f_energy_fibre)


#correlation test
cor.test(combined_data$f_obese, combined_data$f_energy_fibre)
cor.test(combined_data$f_healthy_weight, combined_data$f_energy_fibre)


#regression 
plot(combined_data$f_energy_fibre, combined_data$f_obese, main="Scatterplot")
plot(combined_data$f_energy_fibre, combined_data$f_healthy_weight, main="Scatterplot")

plot(combined_data$f_energy_fibre, combined_data$f_obese, xlab="Percentage of Energy from Fibre", ylab="Percentage of Obese Individuals", main="Scatterplot of how Percentage of Energy from Fibre affects Percentage of Obese Individuals")
lines(lowess(combined_data$f_energy_fibre, combined_data$f_obese))
plot(combined_data$f_energy_fibre, combined_data$f_healthy_weight, xlab="Percentage of Energy from Fibre", ylab="Percentage of Healthy Weight Individuals", main="Scatterplot of how Percentage of Energy From Fibre affects Percentage of Healthy Weight Individuals")
lines(lowess(combined_data$f_energy_fibre, combined_data$f_healthy_weight))


