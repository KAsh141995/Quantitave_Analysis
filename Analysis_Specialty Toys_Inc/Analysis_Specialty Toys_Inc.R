#Case 02

#Question 1

#mean=20000 standard deviation=5000

x <- seq(0,40000,by=1)      #Creating a sequence of sample points 
y <- dnorm(x,20000,5000)    #Plotting normal distribution
plot(x,y)

#Question 2

#computing cumulative probability for each order quantities

pnorm(15000,20000,5000,lower.tail = F)

pnorm(18000,20000,5000,lower.tail = F)

pnorm(24000,20000,5000,lower.tail = F)

pnorm(28000,20000,5000,lower.tail = F)


#Question 3
 
#Created function to compute profit for each case scenario

Projected_Profit<- function(num_units,num_sales)
{
  total_cost <- num_units * 16     
  if (num_units>num_sales) {
    sell_price1<-num_sales*24
    sell_price2<-(num_units-num_sales)*5
    total_sellprice <- sell_price1 + sell_price2 
    }
  else 
  {
    total_sellprice<-num_units *24
  }
  profit<-total_sellprice-total_cost
  returnValue(profit)
}

#Worst case:
Projected_Profit(15000,10000)
Projected_Profit(18000,10000)
Projected_Profit(24000,10000)
Projected_Profit(28000,10000)

#Most likely case:
Projected_Profit(15000,20000)
Projected_Profit(18000,20000)
Projected_Profit(24000,20000)
Projected_Profit(28000,20000)

#Best case:
Projected_Profit(15000,30000)
Projected_Profit(18000,30000)
Projected_Profit(24000,30000)
Projected_Profit(28000,30000)


#Question 4

#Computing re-order point(quantities to order) 

qnorm(0.30,20000,5000,lower.tail = F)  

#Computing profits for each case scenario by using above order-quantity value 

#worst case
Projected_Profit(22622,10000)

#Most likely
Projected_Profit(22622,20000)

#Best case
Projected_Profit(22622,30000)


#Question 5

#Computing projected profit for 19000,20000,21000 units 

#Worst case:
Projected_Profit(19000,10000)
Projected_Profit(20000,10000)
Projected_Profit(21000,10000)

#Most likely case:
Projected_Profit(19000,20000)
Projected_Profit(20000,20000)
Projected_Profit(21000,20000)

#Best case:
Projected_Profit(19000,30000)
Projected_Profit(20000,30000)
Projected_Profit(21000,30000)














