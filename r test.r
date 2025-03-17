# install.packages("WDI")
# install.packages("tidyverse")
# install.packages("plotly")
# install.packages("stargazer")

library(WDI)
library(tidyverse)
library(ggplot2)
library(plotly)
library(stargazer)

Assignment_data<- WDI(country = "CL", indicator = c("NE.IMP.GNFS.KD", "NE.EXP.GNFS.KD", "NY.GDP.MKTP.KD", "PX.REX.REER"), start = 1991,end = 2023, extra = TRUE, cache = NULL)

# Renaming variables
Assignment_data <- Assignment_data %>% rename("Imports" = "NE.IMP.GNFS.KD")
Assignment_data <- Assignment_data %>% rename("Exports" = "NE.EXP.GNFS.KD")
Assignment_data <- Assignment_data %>% rename("GDP" = "NY.GDP.MKTP.KD")
Assignment_data <- Assignment_data %>% rename("REER" = "PX.REX.REER")


Assignment_data <- Assignment_data[,c(3,4,7,8,9,10)]
colSums(is.na(Assignment_data))  # Count missing values per column


# Regressions
Assignment_data <- Assignment_data %>%
  mutate(
    log_Exports = log(Exports),
    log_Imports = log(Imports),
    log_GDP = log(GDP),
    log_REER = log(REER)
  )

# Estimate regression models
reg_exports <- lm(log_Exports ~ log_GDP + log_REER, data = Assignment_data)
reg_imports <- lm(log_Imports ~ log_GDP + log_REER, data = Assignment_data)

# Display regression results
summary(reg_exports)
summary(reg_imports)

stargazer(reg_exports, reg_imports, type = "text")

