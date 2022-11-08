sma_forecast <- function(df, h, m, w = NULL){ 
# Error handling 
  if(h > nrow(df)){
stop("The length of the forecast horizon must be shorter than the length of the series")}
if(m > nrow(df)){
stop("The length of the rolling window must be shorter than the length of the series")} 
if(!is.null(w)){ if(length(w) != m){
stop("The weight argument is not aligned with the length of the rolling window")} else if(sum(w) !=1){
 stop("The sum of the average weight is different than 1") }}
# Setting the average weigths 
if(is.null(w)){ w <- rep(1/m, m)
}
### Setting the data frame ### 
# Changing the Date object column name 
names(df)[1] <- "date"
# Setting the training and testing partition 
# according to the forecast horizon
df$type <- c(rep("train", nrow(df) - h), rep("test", h))
 # Spreading the table by the partition type
 df1 <- df %>% spread(key = type, value = y) 
# Create the target variable
 df1$yhat <- df1$train
# Simple moving average function 
for(i in (nrow(df1) - h + 1):nrow(df1)){
 r <- (i-m):(i-1)
df1$yhat[i] <- sum(df1$yhat[r] * w) }
# dropping from the yhat variable the actual values
 # that were used for the rolling window 
df1$yhat <- ifelse(is.na(df1$test), NA, df1$yhat) 
df1$y <- ifelse(is.na(df1$test), df1$train, df1$test)
return(df1)}