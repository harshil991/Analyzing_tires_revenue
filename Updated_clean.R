df<-read_("C:/Users/harshil/Desktop/aly6080/University_Project_Tires_Cleaned_values_May8.xlsx")
library(readxl)
df <- read_excel("C:/Users/harshil/Desktop/aly6080/University_Project_Tires_Cleaned_values_May8.xlsx")

summary(df)

#missing rates for all variables
sapply(df, function(x) mean(is.na(x)))

#Creating subsets for quantitative vraibales
A1<-subset(df, select = c("Quantity", "Discount", "Total Price", "GP$", "Price per Tire"))
summary(A1)
var(A1)

#percentage of null values in subset
colSums(is.na(A1))
colSums(is.na(A1)) / nrow(A1)

#percentile 
quantile(A1$Quantity, probs = c(0.01,0.02,0.03,0.05,0.1,0.25,0.5,0.75,0.95,0.97,0.98,0.99,0.991,0.995,0.998,0.9991,0.9992,0.9993,0.9992,
                                0.9993,
                                0.9994,
                                0.9995,
                                0.9996,
                                0.9997,
                                0.9998,
                                0.9999,
                                0.99998,
                                0.99999,
                                1))
quantile(A1$Discount, probs = c(0.01,0.02,0.03,0.05,0.1,0.25,0.5,0.75,0.95,0.97,0.98,0.99))

quantile(A1$`Total Price`, probs = c(0.01,0.02,0.03,0.05,0.1,0.25,0.5,0.75,0.95,0.97,0.98,0.99,0.991,0.992,0.993,0.994,0.995,0.9993,
                                     0.9994,
                                     0.9995,
                                     0.9996,
                                     0.9997,
                                     0.9998,
                                     0.9999,
                                     0.99991,
                                     0.99992,
                                     0.99993,
                                     0.99994,
                                     0.99995,
                                     0.99996,
                                     0.99997,
                                     0.99998,
                                     0.99999))
quantile(A1$`Price per Tire`, probs = c(0.01,0.02,0.03,0.05,0.1,0.25,0.5,0.75,0.95,0.97,0.98,0.99))


#Deleting 6 missing values which are basically zero
GP_new<-na.omit(A1$`GP$`)
quantile(GP_new, probs = c(0.01,0.02,0.03,0.05,0.1,0.25,0.5,0.75,0.95,0.97,0.98,0.99))
var(GP_new)
summary(GP_new)


#Proportion of negative values in tire quantity( negative values indicates returns of the product by customer)
negative_tire_qty <- A1[A1$Quantity <= 0, ]
proportion_neg_tire_qty <- nrow(negative_tire_qty) / nrow(A1) * 100
View(negative_tire_qty)
View(proportion_neg_tire_qty)
#Proportion of negative values in tire quantity(Retailers are not likely to purchase more than 10tires so we can just ignore them as of now)
POSITIVE_tire_qty <- A1[A1$Quantity >= 10, ]
proportion_POS_tire_qty <- nrow(POSITIVE_tire_qty) / nrow(A1) * 100
View(POSITIVE_tire_qty)
View(proportion_POS_tire_qty)


#correlation plot

library(corrplot)
library(ggplot2)
library(dplyr)
library("ggpubr")
library(dplyr)
install.packages("ggcorrplot")
library(ggcorrplot)


#removing 6 columns with zero values in GP$ (0 values dosent make any sense in GP$)
A1_new<-na.omit(A1)
summary(A1_new)

#Renaming Price column into Sales 
A1_new <- A1_new %>%
  rename(Sales = `Total Price`)
cor<-corrplot(cor(A1_new), method = "number")



QD<-subset(A1_new, select = c("Quantity", "Discount"))
cor_matrix <- cor(A1_new)
ggcorrplot(cor_matrix, type = "lower", lab = TRUE, lab_size = 3)


#Correlation plot
cor1<-corrplot(cor(A1_new), method = "Pearson")


#QuantityVSPrice
res <- cor.test(A1_new$Quantity,A1_new$Sales , 
                method = "pearson")
res

plot <- ggplot(A1_new, aes(x = Quantity, y = Sales)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Add the correlation coefficient and p-value to the plot title
plot <- plot + 
  ggtitle(paste("Correlation: r = ", round(res$estimate, 2), 
                ", p-value = ", format.pval(res$p.value, digits = 2, 
                                            eps = 0.001, 
                                            style = "traditional",ylab="sales in $"), 
                sep = ""))

plot

################################################################################################################
#QuantityVSDiscount
min(A1_new$Discount)
res1 <- cor.test(A1_new$Quantity,A1_new$Discount , 
                 method = "pearson")
res1
plot1 <- ggplot(A1_new, aes(x = Quantity, y = Discount)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Add the correlation coefficient and p-value to the plot title

plot1 <- plot +
  ylab("Discount") +  
  ggtitle(paste("Correlation: r = ", round(res1$estimate, 2), 
                ", p-value = ", format.pval(res1$p.value, digits = 2, 
                                            eps = 0.001, 
                                            style = "traditional"), 
                sep = ""))
plot1

#QuantityVSGP$
res3 <- cor.test(A1_new$Quantity,A1_new$`GP$` , 
                 method = "pearson")
res3
plot2 <- ggplot(A1_new, aes(x = Quantity, y = `GP$`)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Add the correlation coefficient and p-value to the plot title
plot2 <- plot + 
  ylab("GP$") +
  ggtitle(paste("Correlation: r = ", round(res3$estimate, 2), 
                ", p-value = ", format.pval(res3$p.value, digits = 2, 
                                            eps = 0.001, 
                                            style = "traditional"), 
                sep = ""))
plot2
############################################################################################################################