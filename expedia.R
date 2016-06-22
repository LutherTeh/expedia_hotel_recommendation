library(ggplot2)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(data.table)
library(DescTools)


##please do some exploratory data analysis on small dataset first (e.g. 10%) 
##before proceed to whole train.csv data set which size = 3.7GB
##select useful variables as predictor

expedia_train <- fread('train.csv', header=TRUE, select = c("is_booking","orig_destination_distance","srch_destination_id",
                                   "hotel_cluster"))

expedia_test <- fread('test.csv', header=TRUE)



## function to calculate sum of all rows   
sum_and_count <- function(x){
   sum(x)*0.95 + length(x) *(1-0.95)
  
}


## determine hotel cluster (Y) based on predictor (X1,X2..)
dest_id_hotel_cluster_count <- expedia_train[,sum_and_count(is_booking),by=list(orig_destination_distance, hotel_cluster)]
dest_id_hotel_cluster_count1 <- expedia_train[,sum_and_count(is_booking),by=list(srch_destination_id, hotel_cluster)]

## function to select the top 5 hotel 
top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

dest_top_five <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=orig_destination_distance]
dest_top_five1 <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=srch_destination_id]

## Now move to test set,  predict & recommend the hotel cluster for each of the test set 
dd <- merge(expedia_test,dest_top_five, by="orig_destination_distance",all.x=TRUE)[order(id),list(id,V1)]
dd1 <- merge(expedia_test,dest_top_five1, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]
dd$V1[is.na(dd$V1)] <- dd1$V1[is.na(dd$V1)] 

write.csv(dd,"submission.csv", row.names = FALSE)


