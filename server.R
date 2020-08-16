#-------------------------------------------------------------------------------
#server.R
#-------------------------------------------------------------------------------

if(!require(dplyr))
  install.packages("dplyr")
if(!require(ggthemes))
  install.packages("ggthemes")
if(!require(ggplot2))
  install.packages("ggplot2")
if(!require(corrplot))
  install.packages("corrplot")
if(!require(tidyr)) 
  install.packages("tidyr")


# Read Data
#-------------------------------------------------------------------------------
fulldata <- read.csv("ontario_listings.csv")


# Conditional selecting data and remove na
#-------------------------------------------------------------------------------
house <- subset(fulldata, fulldata$Status=="Sold" & 
		!is.na(fulldata$ListPrice) & 
		!is.na(fulldata$SoldPrice),
                c(Status,Area,Type,Bathrooms,Bedrooms,ParkingTotal,ListPrice,SoldPrice,
                  Taxes,ListDate,SoldDate,ClosedDate,Basement,Garage,Feature,Age))
#View(house)
#check how many records were kept
NROW(house)
NROW(fulldata)
NROW(house)/NROW(fulldata)*100
#change date format
house$ClosedDate<-as.Date(house$ClosedDate)
house$SoldDate<-as.Date(house$SoldDate)
house$ListDate<-as.Date(house$ListDate)
#View(house)
#Grouping by
houseg<- house %>% select(Status,Area,Type,SoldPrice) %>% 
  group_by(Status,Area,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
#View(houseg)


# Basement Data Preparation
#-------------------------------------------------------------------------------
a<- house %>% filter(Type %in% c("Detached","Semi-Detached","Townhouse"))
c<-a
unique(c$Basement)
unique(fulldata$Garage)
unique(fulldata$Feature)

sum(is.na(fulldata$Basement))/nrow(fulldata)
sum(is.na(fulldata$Feature))/nrow(fulldata)
sum(is.na(fulldata$Garage))/nrow(fulldata)

houseb <- c %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Basement,SoldPrice) %>% 
  group_by(Basement) %>% 
  dplyr::summarise(AverageSoldPrice=mean(SoldPrice),count=n())
housec <- houseb %>% 
  filter(Basement %in% c("Apartment","Finished","Part Finished","Unfinished"))


#-------------------------------------------------------------------------------
# Define server logic required to visualization
#-------------------------------------------------------------------------------
server <- function(input, output) {
  output$plot1 <- reactivePlot(function() {

# Plotting chart based on input option
#-------------------------------------------------------------------------------

#1. Bubble plot - Region and House Type effect on House Price and Sales Volume
#-------------------------------------------------------------------------------
if (input$visualizeOption == "1.Region and House Type effect on Price and Sales Volume") {
p1 <- ggplot(houseg, aes(x=Area, y=AveragePrice, size = count,color=Type)) +
   geom_point(alpha=2)+
   scale_size(range = c(.1, 24), name="Sales Count")+
   ylim(200000,1500000)
}


#2. Bubble plot - Region and House Type effect on Sold Price Diff
#-------------------------------------------------------------------------------
if (input$visualizeOption == "2.Region and House Type effect on Sold Price Diff-Bubble") {
houseg1 <- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  group_by(Status,Area,Type) %>%  
  dplyr::summarise(AveragePriceDifference=mean(PriceDifference),count=n())
p1 <- ggplot(houseg1, aes(x=Area, y=AveragePriceDifference, size = count,color=Type)) +
   geom_point(alpha=2)+
   scale_size(range = c(.1, 24), name="Sales Count")+
   ylim(-25000,100000)
}


#3. Box plot - Region and House Type effect on Sold Price Diff
#-------------------------------------------------------------------------------
if (input$visualizeOption == "3.Region and House Type effect on Sold Price Diff-Boxplot") {
houseg11 <- house %>% mutate(PriceDifference=SoldPrice-ListPrice) %>% 
  select(Status,Area,Type,PriceDifference) %>% 
  filter(Type %in% c("Semi-Detached","Detached","Condo-Apartment"))%>% 
  filter(Area %in% c("Toronto","York","Peel","Halton","Durham"))
p1 <- ggplot(data=houseg11,aes(x=Area,y=PriceDifference/1000,fill=Type))+
  geom_boxplot()+ylim(-100,100)
}


#4. Bar chart - Region and House Type effect on Time on Market
#-------------------------------------------------------------------------------
if (input$visualizeOption == "4.Region and House Type effect on Time on Market") {
houseg2 <- house %>% mutate(DateDiff=difftime(SoldDate, ListDate, units = "days")) %>% 
  filter(Type %in% c("Detached","Semi-Detached","Condo-Apartment")) %>% 
  filter(Area %in% c("Toronto","York","Peel","Hamilton","Halton","Durham"))%>% 
  select(Status,Area,Type,DateDiff) %>% group_by(Status,Area,Type) %>%  
  dplyr::summarise(AverageDateDiff=mean(DateDiff),count=n())
#houseg2[order(houseg2$AverageDateDiff),]
#View(houseg2)
p1 <- ggplot(houseg2,aes(fill=Type,x=Area,y=AverageDateDiff)) +
    geom_bar(position="dodge", stat="identity") + 
    ggtitle("Average Date Diff") + 
    xlab("Area") +
    ylab("Average Date Diff") +
    theme_bw()
}


#5. Pie chart - Finished / Unfinished Basement effect on Market Share
#-------------------------------------------------------------------------------
if (input$visualizeOption == "5.Finished / Unfinished Basement effect on Market Share") {
p1 <- ggplot(housec, aes(x="",y=count, fill=Basement))+ 
  geom_bar(stat="identity",width = 1,color="white") +
  coord_polar("y",start=0)+
  theme_void()
}


#6. Bar chart - Finished / Unfinished Basement effect on Sold Price 
#-------------------------------------------------------------------------------
if (input$visualizeOption == "6.Finished / Unfinished Basement effect on Sold Price") {
p1 <- ggplot(housec,aes(x=Basement,y=AverageSoldPrice,fill=Basement)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(breaks=seq(0,1000000,250000)) +
# scale_fill_manual(values = c("Red","blue")) +  
  xlab("Basement") +
  ylab("Average Sold Price") +
  ggtitle("Average Sold Price") + 
  theme_bw()
}


#7. Age effect on Sales Volume and House Price
#-------------------------------------------------------------------------------
if (input$visualizeOption == "7.Age effect on Sales Volume and House Price") {
houseage<- house %>% select(Age,Type,SoldPrice) %>% 
  group_by(Age,Type) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())

houseage1<- house %>% select(Age,SoldPrice) %>% 
  group_by(Age) %>% dplyr::summarise(AveragePrice=mean(SoldPrice),count=n())
houseage2<-na.omit(houseage1)

coeff <- 1000
p1 <- ggplot(houseage2, aes(Age)) +
  geom_line(aes(y=count), colour="blue") +
  geom_line(aes(y=AveragePrice/coeff), colour="red") +  
  scale_y_continuous(name = "Sales Count",
	sec.axis = sec_axis(~.*coeff, breaks =seq(0,4000000,1250000), name = "Average Price"))
}


#Return the Plot chart
#-------------------------------------------------------------------------------
print(p1)
}
)}