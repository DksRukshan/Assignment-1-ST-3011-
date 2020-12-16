install.packages("readr")
#load the library
library(readr)
#import the csv file MobilePrice
MobilePrices <- read_csv("dataset_28[copy]/dataset_28/MobilePrice.csv")
#check the data
head(MobilePrices)
#looking for missing values
missingValues <- colSums(is.na(MobilePrices))
missingValues
#there are no missing values.

#in Mobile prices data set, Mobile phones without a front camera were given 0 value for the amount of mega pixels in front
#camera. When processing the data set a new column was added to represent the availability of the front camera if the front
#camera was available 1,else 0.
Fcavailability <- c()
for (frontC in MobilePrices$fc) {
       if(frontC == 0){
              Fcavailability <- append(Fcavailability,0)
         }else{
              Fcavailability <- append(Fcavailability,1)
         }
}
MobilePrices$fc_availability <- Fcavailability
#in Mobile prices data set, Mobile phones without a primary camera were given 0 value for the amount of mega pixels in primary
#camera. When processing the data set a new column was added to represent the availability of the primary camera if the primary
#camera was available 1,else 0.
Pcavailability <- c()
for (primaryC in MobilePrices$pc) {
       if(primaryC == 0){
             Pcavailability <- append(Pcavailability,0)
         }else{
               Pcavailability <- append(Pcavailability,1)
           }
}
MobilePrices$pc_availability <- Pcavailability
############################################################################################

############################################################################################
#Find duplicate values to check weather the same devise entered more than once.
MobilePrices %>% distinct()
nrow(distinct(MobilePrices)) == nrow(MobilePrices)
#>True.
############################################################################################
#Use ggplot2
library(ggplot2)
############################################################################################

#Univariate Analysis for battery Power.
# 01. Histogram.
ggplot(data=MobilePrices,aes(battery_power))+geom_histogram(bins =50,fill ="cyan3",color="black")+ggtitle("Histogram Of Battery Capacity")+xlab("Battery Capacity (in mAh)")+ylab("Count")

# 02. Area plot.
ggplot(data=MobilePrices,aes(battery_power))+geom_area(stat="bin",color="black",fill = "cyan3")+ggtitle("Area Plot Of Battery Capacity")+xlab("Battery Capacity (in mAh)")+ylab("Count")

#03. Dot Plot.
ggplot(data=MobilePrices,aes(battery_power))+geom_dotplot(binwidth =20,color="black",fill="cyan3")+ggtitle("Dot Plot Of Battery Capacity")+xlab("Battery Capacity (in mAh)")+ylab("Count")

#04. density plot.
ggplot(data=MobilePrices,aes(battery_power))+geom_density(kernel="gaussian",color="cyan3")+ggtitle("Density Plot Of Battery Capacity")+xlab("Battery Capacity (in mAh)")+ylab("Density")
#shows a Uniform distribution
############################################################################################

#Univariate Analysis for Phone Weight
#01. Histogram.
ggplot(data=MobilePrices,aes(mobile_wt))+geom_histogram(bins =30,fill ="cyan3",color="black")+ggtitle("Histogram Of Weight Of The Phone")+xlab("Weight of the phone (in grams)")+ylab("Count")

# 02. Area plot.
ggplot(data=MobilePrices,aes(mobile_wt))+geom_area(stat="bin",color="black",fill = "cyan3",bins=30)+ggtitle("Area Plot Of Weight Of The Phone")+xlab("Weight of the phone (in grams)")+ylab("Count")

#03. Dot Plot.
ggplot(data=MobilePrices,aes(mobile_wt))+geom_dotplot(binwidth =1.5,color="black",fill="cyan3")+ggtitle("Dot Plot Of Weight Of The Phone")+xlab("Weight of the phone (in grams)")+ylab("Count")

#04. density plot.
ggplot(data=MobilePrices,aes(mobile_wt))+geom_density(kernel="gaussian",color="cyan3")+ggtitle("Density Plot Of Weight Of The Phone")+xlab("Weight of the phone (in grams)")+ylab("Density")
#shows a Uniform distribution
#############################################################################################

#Univariate Analysis for Front Camera Quality
#Since 0 value means that the phone doesn't have a Front camera that value was dropped
Front_c <- c()
for(i in MobilePrices$fc){
       if(i != 0){
             Front_c <- append(Front_c,i)
         }
}
df <- data.frame(Front_c)

#01. Histogram.
ggplot(data=df,aes(Front_c))+geom_histogram(bins =10,fill ="cyan3",color="black")+ggtitle("Histogram Of Front Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

# 02. Area plot.
ggplot(data=df,aes(Front_c))+geom_area(stat="bin",color="black",fill = "cyan3",bins=10)+ggtitle("Area Plot Of Front Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

#03. Dot Plot.
ggplot(data=df,aes(Front_c))+geom_dotplot(binwidth =0.05,color="black",fill="cyan3")+ggtitle("Dot Plot Of Front Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

#04. density plot.
ggplot(data=df,aes(Front_c))+geom_density(kernel="gaussian",color="cyan3")+ggtitle("Density Plot Of Front Camera Quality")+xlab("Number of Megapixels")+ylab("Density")

#############################################################################################

#Univariate Analysis for Primary Camera Quality
#Since 0 value means that the phone doesn't have a Primary camera that value was dropped
Primary_c <- c()
for(i in MobilePrices$pc){
  if(i != 0){
    Primary_c <- append(Primary_c,i)
  }
}
df2 <- data.frame(Primary_c)

#01. Histogram.
ggplot(data=df2,aes(Primary_c))+geom_histogram(bins =10,fill ="cyan3",color="black")+ggtitle("Histogram Of Primary Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

# 02. Area plot.
ggplot(data=df2,aes(Primary_c))+geom_area(stat="bin",color="black",fill = "cyan3",bins=10)+ggtitle("Area Plot Of Primary Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

#03. Dot Plot.
ggplot(data=df2,aes(Primary_c))+geom_dotplot(binwidth =0.05,color="black",fill="cyan3")+ggtitle("Dot Plot Of Primary Camera Quality")+xlab("Number of Megapixels")+ylab("Count")

#04. density plot.
ggplot(data=df2,aes(Primary_c))+geom_density(kernel="gaussian",color="cyan3")+ggtitle("Density Plot Of Primary Camera Quality")+xlab("Number of Megapixels")+ylab("Density")

#############################################################################################

#Univariate Analysis for Phone RAM
#01. Histogram.
ggplot(data=MobilePrices,aes(ram))+geom_histogram(bins =30,fill ="cyan3",color="black")+ggtitle("Histogram Of Phone RAM")+xlab("Capacity (in megabytes)")+ylab("Count")

# 02. Area plot.
ggplot(data=MobilePrices,aes(ram))+geom_area(stat="bin",color="black",fill = "cyan3",bins=30)+ggtitle("Area Plot Of Phone RAM")+xlab("Capacity (in megabytes)")+ylab("Count")

#03. Dot Plot.
ggplot(data=MobilePrices,aes(ram))+geom_dotplot(binwidth =60,color="black",fill="cyan3")+ggtitle("Dot Plot Of Phone RAM")+xlab("Capacity (in megabytes)")+ylab("Count")

#04. density plot.
ggplot(data=MobilePrices,aes(ram))+geom_density(kernel="gaussian",color="cyan3")+ggtitle("Density Plot Of Phone RAM")+xlab("Capacity (in megabytes)")+ylab("Density")
#shows a Uniform distribution
#############################################################################################

#Univariate Analysis for Dual Sim Function.
#01. Bar chart.
#Turn coding into avilable and Unavaillable.
dual_s <- c()
for(i in MobilePrices$dual_sim){
     if(i==0){
           dual_s <-append(dual_s,"Unavailable")
       }else{
             dual_s <-append(dual_s,"Available")
         }
}
df_sim <- data.frame(dual_s)
ggplot(df_sim,aes(dual_s))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of Dual Sim Functionality")+xlab("Dual Sim Functionality")+ylab("Count")

#02. Pie chart

############################################################################################

#Univariate Analysis for 4-G Function.
#01. Bar chart.
#Turn coding into avilable and Unavaillable.
four_g <- c()
for(i in MobilePrices$four_g){
  if(i==0){
    four_g <-append(four_g,"Unavailable")
  }else{
    four_g <-append(four_g,"Available")
  }
}
df_fourG <- data.frame(four_g)
ggplot(df_fourG,aes(four_g))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of 4-G Functionality")+xlab("4-G Functionality")+ylab("Count")

#02. Pie chart

############################################################################################

#Univariate Analysis for Touchscreen Function.
#01. Bar chart.
#Turn coding into avilable and Unavaillable.
touch_screen <- c()
for(i in MobilePrices$touch_screen){
  if(i==0){
    touch_screen <-append(touch_screen,"Unavailable")
  }else{
    touch_screen <-append(touch_screen,"Available")
  }
}
df_touchscreen <- data.frame(touch_screen)
ggplot(df_touchscreen,aes(touch_screen))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of Touchscreen Functionality")+xlab("Touchscreen Functionality")+ylab("Count")

#02. Pie chart

############################################################################################

#Univariate Analysis for Price Range.
#01. Bar chart.
#Turn coding into low cost, medium cost, high cost and very high cost
price_rangeV <- c()
for(i in MobilePrices$price_range){
  if(i==0){
    price_rangeV <-append(price_rangeV,"Low Cost")
  }else if(i==1){
    price_rangeV <-append(price_rangeV,"Medium Cost")
  }else if(i==2){
    price_rangeV <-append(price_rangeV,"High Cost")
  }else if(i==3){
    price_rangeV <-append(price_rangeV,"Very High Cost")
  }
}
df_price_range <- data.frame(price_rangeV)
ggplot(df_price_range,aes(price_rangeV))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of Price Range")+xlab("Price Range")+ylab("Count")
#Counts of all the  cost range are equal.
#02. Pie chart

############################################################################################

#Univariate Analysis for Front camera Availability Function.
#01. Bar chart.
#Turn coding into avilable and Unavaillable.
front_cam <- c()
for(i in MobilePrices$fc_availability){
  if(i==0){
    front_cam <-append(front_cam,"Unavailable")
  }else{
    front_cam <-append(front_cam,"Available")
  }
}
df_front_cam <- data.frame(front_cam)
ggplot(df_front_cam,aes(front_cam))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of Front Camera Functionality")+xlab("Front Camera Functionality")+ylab("Count")

#02. Pie chart

############################################################################################

#Univariate Analysis for Primary camera Availability Function.
#01. Bar chart.
#Turn coding into avilable and Unavaillable.
primary_cam <- c()
for(i in MobilePrices$pc_availability){
  if(i==0){
    primary_cam <-append(primary_cam,"Unavailable")
  }else{
    primary_cam <-append(primary_cam,"Available")
  }
}
df_primary_cam <- data.frame(primary_cam)
ggplot(df_primary_cam,aes(primary_cam))+geom_bar(color="black",fill="cyan3")+ggtitle("Bar chart Of Primary Camera Functionality")+xlab("Primary Camera Functionality")+ylab("Count")

#02. Pie chart

############################################################################################
############################################################################################


## Bivariate Analysis.

#01. Price Range vs Battary Power
#turn the price range into factor with order
price_range <- factor(price_rangeV,order=TRUE,levels =c("Low Cost","Medium Cost","High Cost","Very High Cost"))
#Create the data frame 
df_bp <- data.frame(battery_power=MobilePrices$battery_power,price_range)
#1) Box Plot.
ggplot(df_bp,aes(battery_power,price_range))+geom_boxplot(color="black",fill="cyan3")+ggtitle("Box Plot Of Price Range Vs Battery Power")+xlab("Battery Power (in mAh)")+ylab("Cost")
#2) Violin Plot
ggplot(df_bp,aes(battery_power,price_range))+geom_violin(scale="area",color="black",fill="cyan3")+ggtitle("Violin Plot Of Primary Price Range Vs Battery Power")+xlab("Battery Power (in mAh)")+ylab("Cost")
###########################################################################################



#03. Price Range vs Weight Of The Mobile
#turn the price range into factor with order
price_range <- factor(price_rangeV,order=TRUE,levels =c("Low Cost","Medium Cost","High Cost","Very High Cost"))
#Create the data frame 
df_mobile_wt <- data.frame(mobile_wt=MobilePrices$mobile_wt,price_range)
#1) Box Plot.
ggplot(df_fc,aes(mobile_wt,price_range))+geom_boxplot(color="black",fill="cyan3")+ggtitle("Box Plot Of Price Range Vs Weight Of The Mobile")+xlab("Weight Of The Mobile (in grams)")+ylab("Cost")
#2) Violin Plot
ggplot(df_fc,aes(mobile_wt,price_range))+geom_violin(scale="area",color="black",fill="cyan3")+ggtitle("Violin Plot Of Price Range Vs Weight Of The Mobile")+xlab("Weight Of The Mobile (in grams)")+ylab("Cost")
###########################################################################################


#04. Price Range vs Primary camera Megapixcel
#turn the price range into factor with order
price_range <- factor(price_rangeV,order=TRUE,levels =c("Low Cost","Medium Cost","High Cost","Very High Cost"))
#Create the data frame 
df_pc <- data.frame(pc=MobilePrices$pc,price_range)
df_pc <- filter(df_pc, pc !=0) # you might need to load library(dplyr)
#1) Box Plot.
ggplot(df_pc,aes(pc,price_range))+geom_boxplot(color="black",fill="cyan3")+ggtitle("Box Plot Of Price Range Vs Primary Camera quality")+xlab("Camera quality (in Megapixcel)")+ylab("Cost")
#2) Violin Plot
ggplot(df_pc,aes(pc,price_range))+geom_violin(scale="area",color="black",fill="cyan3")+ggtitle("Violin Plot Of Price Range Vs Primary Camera quality")+xlab("Camera quality (in Megapixcel)")+ylab("Cost")
###########################################################################################


#05. Price Range vs RAM Capacity
#turn the price range into factor with order
price_range <- factor(price_rangeV,order=TRUE,levels =c("Low Cost","Medium Cost","High Cost","Very High Cost"))
#Create the data frame 
df_ram <- data.frame(ram=MobilePrices$ram,price_range)
#1) Box Plot.
ggplot(df_ram,aes(ram,price_range))+geom_boxplot(color="black",fill="cyan3")+ggtitle("Box Plot Of Price Range Vs RAM Capacity")+xlab("RAM Capacity (in Megabytes)")+ylab("Cost")
#2) Violin Plot
ggplot(df_ram,aes(ram,price_range))+geom_violin(scale="area",color="black",fill="cyan3")+ggtitle("Violin Plot Of Price Range Vs RAM Capacity")+xlab("RAM Capacity (in Megabytes)")+ylab("Cost")
###########################################################################################

#06.Price Range Vs Dual sim ******Same thing Can Easily done by using "table(MobilePrices$price_range,MobilePrices$dual_sim)"" **********
#Creating Contingency Table.
ct_dualSim <- data.frame(
  available=c(
    (as.data.frame(count(filter(MobilePrices, dual_sim==1 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==1 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==1 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==1 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==1))))$n[1]
    ),
  unavailable=c(
    (as.data.frame(count(filter(MobilePrices, dual_sim==0 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==0 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==0 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==0 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, dual_sim==0))))$n[1]
  ),
  total=c(0,0,0,0,0),
  row.names=c("Low Price","Medium Price","High Price","Very High Price","Total"))
ct_dualSim$total = rowSums(ct_dualSim)
#########################################################################################

#07.Price Range Vs 4-G
#Creating Contingency Table.
ct_fourG <- data.frame(
  available=c(
    (as.data.frame(count(filter(MobilePrices, four_g==1 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==1 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==1 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==1 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==1))))$n[1]
  ),
  unavailable=c(
    (as.data.frame(count(filter(MobilePrices, four_g==0 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==0 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==0 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==0 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, four_g==0))))$n[1]
  ),
  total=c(0,0,0,0,0),
  row.names=c("Low Price","Medium Price","High Price","Very High Price","Total"))
ct_fourG$total = rowSums(ct_fourG)
#########################################################################################
#08.Price Range Vs Touchscreen
#Creating Contingency Table.
ct_touchscreen <- data.frame(
  available=c(
    (as.data.frame(count(filter(MobilePrices, touch_screen==1 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==1 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==1 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==1 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==1))))$n[1]
  ),
  unavailable=c(
    (as.data.frame(count(filter(MobilePrices, touch_screen==0 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==0 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==0 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==0 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, touch_screen==0))))$n[1]
  ),
  total=c(0,0,0,0,0),
  row.names=c("Low Price","Medium Price","High Price","Very High Price","Total"))
ct_touchscreen$total = rowSums(ct_touchscreen)
#########################################################################################

#09.Price Range Vs Front Camera Functionality
#Creating Contingency Table.
ct_fcFunction <- data.frame(
  available=c(
    (as.data.frame(count(filter(MobilePrices, fc_availability==1 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==1 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==1 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==1 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==1))))$n[1]
  ),
  unavailable=c(
    (as.data.frame(count(filter(MobilePrices, fc_availability==0 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==0 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==0 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==0 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, fc_availability==0))))$n[1]
  ),
  total=c(0,0,0,0,0),
  row.names=c("Low Price","Medium Price","High Price","Very High Price","Total"))
ct_fcFunction$total = rowSums(ct_fcFunction)
#########################################################################################

#10.Price Range Vs Primary Camera Functionality
#Creating Contingency Table. 
ct_pcFunction <- data.frame(
  available=c(
    (as.data.frame(count(filter(MobilePrices, pc_availability==1 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==1 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==1 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==1 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==1))))$n[1]
  ),
  unavailable=c(
    (as.data.frame(count(filter(MobilePrices, pc_availability==0 & price_range == 0))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==0 & price_range == 1))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==0 & price_range == 2))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==0 & price_range == 3))))$n[1],
    (as.data.frame(count(filter(MobilePrices, pc_availability==0))))$n[1]
  ),
  total=c(0,0,0,0,0),
  row.names=c("Low Price","Medium Price","High Price","Very High Price","Total"))
ct_pcFunction$total = rowSums(ct_pcFunction)
#########################################################################################
#write all the contingency table files to csv
write.csv(ct_dualSim,"Ct_dualsim.csv", row.names = TRUE)
write.csv(ct_fcFunction,"ct_fcFunction.csv", row.names = TRUE)
write.csv(ct_fourG,"Ct_fourG.csv", row.names = TRUE)
write.csv(ct_pcFunction,"Ct_pcFunction.csv", row.names = TRUE)
write.csv(ct_touchscreen,"Ct_touchscreen.csv", row.names = TRUE)


