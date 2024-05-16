library(tidyverse)
library(caret)
library(GGally)
library(corrplot)
data = read_csv("scaled_clean_tendency_data.csv")
weights = prcomp(data[,3:ncol(data)])

pcframe = data.frame(weights$x)
pcframe = cbind(data[, 1:2], pcframe)

ggplot(pcframe, aes(x = PC1, y = PC2, color = councilmember))
cum_var = cumsum(weights$sdev^2 / sum(weights$sdev^2))
cum_var

ggplot(data, aes(x = date, y = `business`, color = `public safety`)) +
  geom_point()

ggcorr(data[,3:ncol(data)])

corrplot(cor(data[,3:ncol(data)]), method = "shade")

mean_intentions = data[,2:ncol(data)] %>%
  group_by(councilmember) %>%
  summarize_if(is.numeric, mean)