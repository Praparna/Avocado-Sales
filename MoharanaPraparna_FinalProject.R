#Final_Project_Avocado

#Load libraries
library(tidyverse)
library(data.table)
library(magrittr)
library(scales)
library(reshape2)
library(table1)
library(psych)
library(gmodels)
library(lubridate)
library(ggpubr)
library(webr)

#Settting Work Directory
setwd("C:/Users/NEHA/Desktop/MPS/ALY6010/Final Project/avocado.csv")
getwd()

#---------------------------------------------------------------

#Data reading and cleaning

#Reading the file
data_temp1 <- fread("avocado_Final.csv", drop = c("Var"))
view(data_temp1)

#To check structure
str(data_temp11)
summary(data_temp1)

#To check if there are empty values
is_empty(data_temp1)
is.na(data_temp1)

#change few column names
names(data_temp1)[names(data_temp1) == "AveragePrice"] <-
  "Avg_Price"
names(data_temp1)[names(data_temp1) == "Total Volume"] <-
  "Total_Volume"
names(data_temp1)[names(data_temp1) == "4046"] <-
  "Cat1_Sales"
names(data_temp1)[names(data_temp1) == "4225"] <-
  "Cat2_Sales"
names(data_temp1)[names(data_temp1) == "4770"] <-
  "Cat3_Sales"
names(data_temp1)[names(data_temp1) == "Total Bags"] <-
  "Total_Bags"
names(data_temp1)[names(data_temp1) == "Small Bags"] <-
  "Small_Bags"
names(data_temp1)[names(data_temp1) == "Large Bags"] <-
  "Large_Bags"
names(data_temp1)[names(data_temp1) == "XLarge Bags"] <-
  "XLarge_Bags"
names(data_temp1)[names(data_temp1) == "type"] <-
  "Type"
names(data_temp1)[names(data_temp1) == "year"] <-
  "Year"
names(data_temp1)[names(data_temp1) == "region"] <-
  "Region"


# Drop Region Total and Total US
data_final <- data_temp1[!(
  data_temp1$Region == "California" |
    data_temp1$Region == "Plains" |
    data_temp1$Region == "SouthCentral" |
    data_temp1$Region == "RestOfUS" |
    data_temp1$Region == "California" |
    data_temp1$Region == "GreatLakes" |
    data_temp1$Region == "Southeast" |
    data_temp1$Region == "West" |
    data_temp1$Region == "Midsouth" |
    data_temp1$Region == "Northeast"
),]


view(data_final)
str(data_final)


data_final$Cat1_Sales <- round(data_final$Cat1_Sales, 0)
data_final$Cat2_Sales <- round(data_final$Cat2_Sales, 0)
data_final$Cat3_Sales <- round(data_final$Cat3_Sales, 0)
data_final$Total_Bags <- round(data_final$Total_Bags, 0)
data_final$Small_Bags <- round(data_final$Small_Bags, 0)
data_final$Large_Bags <-
  Large_Bags <- round(data_final$Large_Bags, 0)
data_final$XLarge_Bags <- round(data_final$XLarge_Bags, 0)

view(data_final)
summary(data_final)

#--------------------------------------------------------------
#Genarating summary table
summary(data_final)
#summary table
summary_table <-
  round(describe(
    data_final,
    na.rm = TRUE,
    skew = FALSE,
    ranges = TRUE
  ), 2)
summary_table

#exporting table to Txt file
write.table(summary_table,
            file = "Milestone2_Table1.csv",
            sep = ",", )
#-------------------------------------------------------------------------------

# Histogram for Total Avg Price  Distribution

summary(data_final$Avg_Price)

ggplot(data_final, aes(x = Avg_Price)) +
  geom_histogram(binwidth = 0.3,
                 color = "black",
                 fill = "light Blue") +
  geom_vline(
    aes(xintercept = mean(Avg_Price)),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  ggtitle("Avg Price Distribution") + xlab("Price(in $)") +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  theme_bw()
#-------------------------------------------------------------------------------
#histogram with mean lines for sample
hist(
  data_final$Avg_Price,
  main = "Price Distribution",
  col.main = "cadetblue",
  xlab = "Item MRP",
  xlim = c(0, 4),
  col = "cadetblue",
  title=" Price Distribution"
  
)

#to plot mean lines of Total Volume Sales
abline(v = mean(data_final$Avg_Price),
       col = "green",
       lwd = 3)

abline(v = median(data_final$Avg_Price),
       col = "red",
       lwd = 3)

#calculate mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(data_final$Avg_Price)
result


#Add legend to  plot
legend(
  "topright",
  legend = c("Mean", " Median","Mode"),
  col = c("green", "red", "Orange"),
  lty = 1,
  cex = 0.8,
  title = "Line",
  text.font = 4
)
#--------------------------------------------------------------------
#Histogram to observe skewness
ggplot(data_final, aes(Total_Volume)) +
  geom_histogram( fill = "cadet blue") +
  geom_vline(
    aes(xintercept = mean(Total_Volume)),
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  ggtitle("Total Sales Distribution") + xlab("Total Volume Sales (in lbs)") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) + theme_bw()

summary(data_final$Total_Volume)
#__________________________________________
# Box for Total Volume sold 

ggplot(data_final, aes(x = "", y = Total_Volume)) +
  ggtitle("Total Volume sold ") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  ylab("Total Volume") +
  geom_boxplot(outlier.colour = "red")+
  theme_bw()
summary(data_final$Total_Volume)

#-------------------------------------------------------------------------------

# year wise mean price 

ggplot(data_final, aes(x = factor(Year), y = Avg_Price, fill = Year)) +
  ggtitle("Year wise Avg Price") +
  xlab("Year") + ylab("Avg Price (in $)") +
  geom_boxplot(outlier.colour = "red")+
  theme_bw()

#---------------------------------------------------------------------------
# Volume trend
# Change format of Date
data_final$Date <- as.Date(data_final$Date, "%d-%m-%Y")
view(data_final)

# group dates by months
vol_month <- data_final %>%
  group_by(Date = floor_date(Date, unit = "month")) %>%
  summarise(sum_Total_Vol = sum(Total_Volume),
            Mean_Avg_Price=mean(Avg_Price),
            Vol_org = sum(ifelse(Type == "organic", Total_Volume, 0)),
            Vol_con = sum(ifelse(Type == "conventional", Total_Volume, 0)),
            Avg_org=mean(ifelse(Type == "organic", Avg_Price, 0)),
            Avg_con=mean(ifelse(Type == "conventional", Avg_Price, 0)),
            Vol_Cat1 = sum(Cat1_Sales),
            Vol_Cat2 = sum(Cat2_Sales),
            Vol_Cat3 = sum(Cat3_Sales),
            sum_small=sum(Small_Bags),
            sum_large=sum(Large_Bags),
            sum_Xlarge=sum(XLarge_Bags),
            Type=Type)

# Line plot for Volume trend
Trend1 <-ggplot(vol_month, aes(x = Date)) +
  geom_line(aes(y = sum_Total_Vol))  +
  ggtitle("Volume of Avocado Sold - Trend ") +
  xlab("Months") + ylab("Volumes Sold(in lbs) ") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
Trend1

#line graph for avg price trend 
Trend2<-ggplot(vol_month, aes(x = Date)) +
  geom_line(aes(y = Mean_Avg_Price ))  +
  ggtitle("Avg Price of Avocado Sold - Trend ") +
  xlab("Months") + ylab("Avg Price(in $) ") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
Trend2

ggarrange(Trend1, Trend2, ncol = 1, nrow = 2)

#--------------------------------------------------------------------------------------
#line graph for category wise
Trend3<-ggplot(vol_month, aes(x = Date )) +
  geom_line(aes(y = Vol_Cat1 , color="Category 1"))  +
  geom_line(aes(y = Vol_Cat2,color="Category 2"  ))+
  geom_line(aes(y = Vol_Cat3, color="Category 3"))+
  ggtitle("Avocado Sold based on Category ") +
  xlab("Months") + ylab(" Volume Sales (in lbs) ") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
Trend3

#line graph for bag wise trend
Trend4<-ggplot(vol_month, aes(x = Date )) +
  geom_line(aes(y = sum_small , color="Small Bags"))  +
  geom_line(aes(y = sum_large,color="Large Bags"  ))+
  geom_line(aes(y = sum_Xlarge, color="XL Bags"))+
  ggtitle("Avocado Sold based on Bag size ") +
  xlab("Months") + ylab("Volume Sales (in lbs) ") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
Trend4

ggarrange(Trend3, Trend4, ncol = 1, nrow = 2)


#------------------------------------------------------------------------------
#Sales trend organic and conventional 
trend5<-ggplot(vol_month, aes(x = Date, colour=Type)) +
  geom_line(aes(y = Vol_org, color = "Organic")) +
  geom_line(aes(y = Vol_con, color = "Conventional")) +
  ggtitle("Volume Sold - Organic and conventional") +
  xlab("Months") + ylab("Total Volume Sold (in lbs) ") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme(legend.title = element_text(size = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))

#price trend organic and conventional 
trend6<-ggplot(vol_month, aes(x = Date, colour=Type)) +
  geom_line(aes(y = Avg_org, color = "Organic")) +
  geom_line(aes(y = Avg_con, color = "Conventional")) +
  ggtitle("Average price - Organic and conventional") +
  xlab("Months") + ylab("Average price (in $)") +
  scale_x_date(date_label = "%b %y", date_breaks = "3 month") +
  theme(legend.title = element_text(size = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))

ggarrange(trend5, trend6, ncol = 1, nrow = 2)


#-----------------------------------------------------------------------
# One year trend for seasonality
year16<-subset(data_final, Year== 2016)

vol_month16 <- year16 %>%
  group_by(Date = floor_date(Date, unit = "month")) %>%
  summarise(year16_sales = sum(Total_Volume),
            year16_Price=mean(Avg_Price))


Trend7 <-ggplot(vol_month16, aes(x = Date)) +
  geom_line(aes(y = year16_sales))  +
  ggtitle("Volume of Avocado Sold : Trend 2016 ") +
  xlab("Months") + ylab("Volume Sales (in lbs) ") +
  scale_x_date(date_label = "%b", date_breaks = "1 month") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, angle = 30))
Trend7

scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
#---------------------------------------------------------------------------


# Bar plot for Avocados Sales by Region based on Type

#group Total Volumes sold by regions
Type_data <- data_final %>%
  group_by(Region, Type) %>%
  summarise(sum_Vol_type = sum(Total_Volume))
Type_data

ggplot(Type_data, aes(y = sum_Vol_type,
                      x = reorder(Region, sum_Vol_type))) +
  geom_bar(position = "stack",
           stat = "identity",
           fill = "cadet blue") +
  ggtitle("Avocado Sale in Regions by Type") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))  +
  theme_bw() +
  ylab("Volume Avocado Sold") + xlab("Regions")+
theme(
  panel.border = element_blank(),
  panel.grid = element_blank() ,
  axis.text.x  = element_text(
    angle = 90,
    vjust = 0.5
  )) + coord_flip()+theme(axis.text.y = element_text(size = rel(0.8)))

#----------------------------------------------------------------------------
# bar plot for Top 20 regions

# Sort engagement index total in descending order
Region_Descend <- Type_data[order(-Type_data$sum_Vol_type),]
#select Top 20 products by engagement index
Top20 <- head(Region_Descend, 20)
Top20

#Plot top 20 regions
ggplot(data = Top20, aes(x = reorder(Region,sum_Vol_type), y = sum_Vol_type)) +
  ggtitle("Top 20 Regions") +
  geom_bar(stat = "identity", fill = "cadet Blue") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank() ,
    axis.text.x  = element_text(
      size = 8,
      angle = 90,
      vjust = 0.5
    )
  ) +
  xlab("Regions") + ylab("Volumes Sold (in lbs)") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+ coord_flip()
#----------------------------------------------------------------------------------

Year_Sales <-
  subset(data_final, Year == 2015 |
           Year == 2016 |Year == 2017|Year == 2018)

label(Year_Sales$Avg_Price) <- 'Avg_Price'
label(Year_Sales$Total_Volume) <- 'Total_Volume'

table1( ~ Avg_Price + Total_Volume | Year, data = Year_Sales)
#-----------------------------------------------------------------------------------



#Scatter plot for Small_Bags and Avg_Price

ggplot(data_final, aes(y = Small_Bags, x = Avg_Price)) + 
  geom_point() + scale_y_continuous(labels = unit_format(unit = "LBS", scale = 1e-1))


ggplot(data_final, aes(y = Large_Bags, x = Avg_Price)) + 
  geom_point() + scale_y_continuous(labels = unit_format(unit = "LBS", scale = 1e-1))


ggplot(data_final, aes(y = XLarge_Bags, x = Avg_Price)) + 
  geom_point() + scale_y_continuous(labels = unit_format(unit = "LBS", scale = 1e-1))
#-----------------------------------------------------------------
#correlation

continuous_var <-
  cbind(
    Avg_Price = data_final$Avg_Price,
    cat1 = data_final$Cat1_Sales,
    cat2 = data_final$Cat2_Sales,
    cat3 = data_final$Cat3_Sales,
    Small = data_final$Small_Bags,
    Large = data_final$Large_Bags,
    XLarge = data_final$XLarge_Bags
  )

continuous_var

cor1<-cor(continuous_var)


# Visualize
melted_cor <- melt(cor1)
melted_cor$value <- round(melted_cor$value, 2)
head(melted_cor)

# Correlation heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  ggtitle("Correlation Heatmap") +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    vjust = 1,
    angle = 45,
    size = 10,
    hjust = 1
  )) + geom_text(aes(Var2, Var1, label = value),
                 color = "black",
                 size = 3)


#To check correlation between them
continuous_var <-
  cbind(
    Avg_Price = data_final$Avg_Price,
    Small = data_final$Small_Bags,
    Large = data_final$Large_Bags,
    XLarge = data_final$XLarge_Bags
  )
cor(continuous_var)

model <-
  lm(Avg_Price ~ Small_Bags + Large_Bags + XLarge_Bags, data = data_final)
summary(model)

#____________________________________________________________________


# Test 1 : correlation for avg price & Volume

#sample
sample_data <- sample_n(data_final, 40, replace = FALSE)

# subset
cor_data1 <-
  select(sample_data,
         Total_Volume,
         Avg_Price)
head(cor_data1)
summary(cor_data1)

#check normality
shapiro.test(cor_data1$Total_Volume)
shapiro.test(cor_data1$Avg_Price)

#plot normality
# weight
n1 <-
  ggqqplot(cor_data1$Total_Volume, ylab = "Total_Volume") + 
  scale_y_continuous(labels = unit_format(unit = "LBS", scale = 1e-1))
# MRP
n2 <- ggqqplot(cor_data1$Avg_Price, ylab = "Avg_Price")

ggarrange(n1, n2, ncol = 2, nrow = 1)


library("ggpubr")


# Correlation of avg price & Volume

#correlation matrix
install.packages("Hmisc")
library(Hmisc)

cor1 <- round(cor(cor_data1), 2)
cor1


#P value for correlation
test1 <- rcorr(as.matrix(cor_data1))
test1

# Visualize
melted_cor <- melt(cor1)
melted_cor$value <- round(melted_cor$value, 2)
head(melted_cor)

# Correlation matrix
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  ggtitle("Correlation Heatmap") +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    vjust = 1,
    angle = 45,
    size = 10,
    hjust = 1
  )) + geom_text(aes(Var2, Var1, label = value),
                 color = "black",
                 size = 3)

#Visualize data using scatter plot
ggscatter(
  cor_data1,
  x = "Avg_Price",
  y = "Total_Volume",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  xlab = "Avg_Price",
  ylab = "Total Volume Sold"
) + scale_y_continuous(labels = unit_format(unit = "LBS", scale = 1e-1))

#-----------------------------------------------------------------
#Test 2 :  Regression for Avg_Price and Total Volume Sold

# Split the data into training and test set
set.seed(1)
#sample
sample_data <- sample_n(data_final, 50, replace = FALSE)

# subset
reg_data1 <-
  dplyr::select(sample_data,
         Total_Volume,
         Avg_Price)
head(reg_data1)


library(caret)
training.sample2 <- reg_data1$Total_Volume %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data2  <- reg_data1[training.sample2,]
test.data2 <- reg_data1[-training.sample2,]

summary(train.data2)
summary(test.data2)

#normality
shapiro.test(train.data2$Total_Volume)

t_log<-log(train.data2$Total_Volume)
t_log

#histogram to visualize normality
# TotalVoulume sales  Distribution
hist(
  t_log,
  main = "Distribution of total volume sales",
  col.main = "cadetblue",
  xlab = "Total Sales",
  col = "cadetblue",
) 

#to plot mean lines of Items Sold
abline(v = mean(t_log),
       col = "green",
       lwd = 3)


# Build the model
library(e1071)
library(openintro)

model2 <- lm(log1p(Total_Volume) ~ log1p(Avg_Price), data = train.data2)

# Summarize the model
summary(model2)

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

mean(train.data2$Total_Volume)

exp(1.693)

# Make predictions on test data
prediction1 <- round(model2 %>% predict(test.data2), 0)
head(prediction1, 10)


#create data frame of actual and predicted values
values <- data.frame(actual=test.data2$Total_Volume, predicted=exp(prediction1))
round(head(values,5),0)

# plot actual vs predicted
plot(x=exp(prediction1), y=test.data2$Total_Volume,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')


#add diagonal line for estimated regression line
abline(a=0, b=1)

ggplot(test.data2, aes(y=Total_Volume, x=exp(prediction1))) +
  geom_point()+
  scale_y_continuous(name = "Actual Values", labels = comma) +
  scale_x_continuous(name = "Predicted Values", labels = comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+theme_bw()+
  ggtitle("Predicted vs. Actual Values")+geom_smooth(method='lm', se=FALSE)

#-----------------------------------------------------------------------------

#Subset created of Los Angeles
Region<-subset(data_final,Region == 'LosAngeles')
head(Region,2)

# subset
sample_Region <- Region %>%
  group_by(Year) %>%
  summarise(
   Total_Sales = sum(Total_Volume),
    Avg_price = mean(Avg_Price)
    )
head(sample_Region)


cor2 <- round(cor(sample_Region), 2)
cor2


# Visualize
melted_cor <- melt(cor2)
melted_cor$value <- round(melted_cor$value, 2)
head(melted_cor)


# Correlation matrix
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  ggtitle("Correlation Heatmap") +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    vjust = 1,
    angle = 45,
    size = 10,
    hjust = 1
  )) + geom_text(aes(label = value),
                 color = "black",
                 size = 3) +theme(
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank())



#LinearRegression prediction for a model

model <- lm(Total_Volume ~ Avg_Price + Year, data = sample_Region)
summary(model)


#-----------------------------------------------------
#multi regression

require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(scales)
# Multi Linear Regression Line Model
MultiRegModel <- lm(Total_Volume ~ Avg_Price + Year, data = data_final)
summary(MultiRegModel)

ggPredict(MultiRegModel)

dat <- ggPredict(model,terms = c("Avg_Price", "Year"),se=TRUE)


plot(dat)+ scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-4))

#--------------------------------------------------------------
#multi regression 2 

#subset NY
NewYork<- subset(data_final, Region=="NewYork")
str(NewYork)

#total sales vs avg price visualise
ggplot(NewYork, aes(x = Avg_Price, y = Total_Volume, color = Type)) +
  geom_point() + geom_jitter(aes(colour = Type)) +
  ggtitle("Total sales vs Avg Price") +
  theme_bw() +
  theme(axis.text.x = element_text(
    size = 8,
    angle = 45,
    vjust = 0.5
  )) + scale_fill_brewer(palette = "BuGn")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-4))

#sample
set.seed(1)
#sample
sample_data <- sample_n(NewYork, 50, replace = FALSE)

library(caret)
training.sample <- sample_data$Total_Volume%>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- sample_data[training.sample,]
test.data<- sample_data[-training.sample,]

summary(train.data)
summary(test.data)

#normality
shapiro.test(train.data$Total_Volume)

t_log<-log(train.data$Total_Volume)
t_log

#histogram to visualize normality
# TotalVoulume sales  Distribution
hist(
  t_log,
  main = "Distribution of total volume sales",
  col.main = "cadetblue",
  xlab = "Total Sales",
  col = "cadetblue",
) 

#to plot mean lines of Items Sold
abline(v = mean(t_log),
       col = "green",
       lwd = 3)


# Build the model
model6 <- lm(Total_Volume~Avg_Price+ Type , data = train.data)
# Summarize the model
summary(model6) 

# check dummy variables
res <- model.matrix(~Type, data = train.data)
head(res[, -1])

mean(train.data$Total_Volume)

equation1=function(x){coef(model6)[2]*x+coef(model6)[1]}
equation2=function(x){coef(model6)[2]*x+coef(model6)[1]+coef(model6)[3]}

ggplot(NewYork,aes(y=Total_Volume,x=Avg_Price,color=Type))+
  geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(3)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(3)[2])+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-4))+
  theme_bw()

#-----prediction
install.package("ggiraphExtra")
library(ggiraphExtra)

ggPredict(model6,interactive=TRUE)


# Make predictions on test data
prediction <- round(model6 %>% predict(test.data), 0)
head(prediction, 10)

#create data frame of actual and predicted values
values <- data.frame(actual=test.data$Total_Volume, predicted=prediction)
round(head(values,5),0)

# plot actual vs predicted
plot(x=prediction, y=test.data$Total_Volume,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')

#add diagonal line for estimated regression line
abline(a=0, b=1)

#--------------------------------------------------------------------------

# Test 1#########ONE SAMPLE T test FOR AVERAGE PRICE######

summary(data_final$Avg_Price)


sample_Avg_Price <- data_final %>%
  sample_n(50, replace = FALSE)

View(sample_Avg_Price)

#box plot for sample avg price
ggboxplot(
  sample_Avg_Price$Avg_Price,
  ylab = "Avg_Price ($)",
  xlab = FALSE,
  ggtheme = theme_minimal(),
  fill = 'orange'
) + theme_bw()


#histogram for avg price
hist(
  sample_Avg_Price$Avg_Price,
  main = "Avg Price Distribution",
  col.main = "cadetblue",
  xlab = "Item MRP",
  xlim = c(0, 4),
  col = "cadetblue",
)

#to plot mean lines of avocado sold for sample
abline(
  v = mean(sample_Avg_Price$Avg_Price),
  col = "green",
  lwd = 3
)

#to plot hypothetical mean lines of avocado sold
abline(v = mean(data_final$Avg_Price),
       col = "blue",
       lwd = 3)

#Add legend to  plot
legend(
  "topright",
  legend = c("Sample Mean", "Hypothesised Mean"),
  col = c("green", "blue"),
  lty = 1,
  cex = 0.8,
  title = "Line",
  text.font = 4
)

# comparing mean of sample and population
mean(data_final$Avg_Price)
mean(sample_Avg_Price$Avg_Price)

#Normality Test
shapiro.test(sample_Avg_Price$Avg_Price)

ggqqplot(sample_Avg_Price$Avg_Price,
         ylab = "Avg_Price($)",
         ggtheme = theme_minimal())

avg <- t.test(sample_Avg_Price$Avg_Price, mu = 1.4)
avg
plot(avg)
plot(t.test(sample_Avg_Price$Avg_Price, mu = 1.4))
#-------------------------------------------------------------------------

summary(data_final$Total_Volume)

#Test 2 .#####ONE SAMPLE t test FOR VOLUME

ggboxplot(
  data_final$Total_Volume,
  ylab = "Volume(LBS)",
  xlab = FALSE,
  ggtheme = theme_minimal(),
  fill = 'orange',
  yscale = "log2",
  format.scale = TRUE
) + theme_bw()

sample_Volume <- data_final %>%
  sample_n(50, replace = FALSE)

ggboxplot(
  sample_Volume$Total_Volume,
  ylab = "Volume(lbs)",
  xlab = FALSE,
  ggtheme = theme_minimal(),
  fill = 'cadet blue',
  yscale = "log2",
  format.scale = TRUE
) + theme_bw()

mean(data_final$Total_Volume)
mean(sample_Volume$Total_Volume)

#histogram with mean lines for sample
hist(
  sample_Volume$Total_Volume,
  main = "Volume Avocado sold Distribution",
  col.main = "cadetblue",
  xlab = "Volume Sold",
  col = "cadetblue",
)

#to plot mean lines of Volume sold for sample
abline(
  v = mean(sample_Volume$Total_Volume),
  col = "green",
  lwd = 3
)

# plot line of hypothetical mean
abline(v = 200000,
       col = "red",
       lwd = 3)

#Add legend to  plot
legend(
  "topright",
  legend = c("Sample Mean", "Hypothesised Mean"),
  col = c("green", "red"),
  lty = 1,
  cex = 0.8,
  title = "Line",
  text.font = 4
)

shapiro.test(sample_Volume$Total_Volume)

ggqqplot(
  sample_Volume$Total_Volume,
  ylab = "Volume(LBS)",
  yscale = "log2",
  ggtheme = theme_minimal()
)

vol <-
  wilcox.test(sample_Avg_Price$Total_Volume,
              mu = 200000,
              alternative = "less")
vol

# changing confidence interval to 99%
vol1 <- wilcox.test(sample_Avg_Price$Total_Volume,
                    mu = 200000,
                    alternative = "less" , conf.int = .99)
vol1


#---------------------------------------------------------------------

#Test 3: two sample t test for comparing mean of price based on
#organic and conventional type

#summary of price w.r.t.Type
library(table1)
table1( ~ Avg_Price | Type , data = data_final)
# Fat content wise MRP of population
ggplot(data_final, aes(Type, Avg_Price, fill = Type)) +
  geom_boxplot() + theme_bw()


#extract a sample
sample_Price <- data_final %>%
  group_by(Type) %>%
  sample_n(50, replace = FALSE)

view(sample_Price)
str(sample_Price)

#summary table
table1(~ Avg_Price | Type, data = sample_Price)

Conventional <- subset(sample_Price, Type == "conventional")
summary(Conventional$Avg_Price)

Organic <- subset(sample_Price, Type == "organic")
summary(Organic$Avg_Price)

# Fat content wise MRP
ggplot(sample_Price, aes(Type, Avg_Price, fill = Type)) +
  geom_boxplot() + theme_bw()

#Histogram to observe skewness
ggplot(sample_Price, aes(Avg_Price)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap( ~ Type) + theme_bw()


# visually check normality

a <-
  ggqqplot(Conventional$Avg_Price,
           ylab = "Avg_Price for conventional",
           ggtheme = theme_minimal())

b <- ggqqplot(Organic$Avg_Price, ylab = "Avg_Price for Organic",
              ggtheme = theme_minimal())

ggarrange(a, b, ncol = 2, nrow = 1)

#test normality of sample Low Fat
shapiro.test(Conventional$Avg_Price)
#test normality of sample regular
shapiro.test(Organic$Avg_Price)

# variance F test

var.test(Avg_Price ~ Type,
         data = sample_Price,alternative = "two.sided")

#-----------------------------------------------------------
# Test 4 : Two sample t-test for unequal variance


t1 <-
  t.test(
    Avg_Price ~ Type,
    data = sample_Price,
    alternative = "two.sided",
    var.equal = FALSE
  )
t1
plot(t1)

# one tailed test
plot(
  t2 <-
    t.test(
      Avg_Price ~ Type,
      data = sample_Price,
      alternative = "greater",
      var.equal = FALSE
    )
)


#-----------------------------------------------------------------------------