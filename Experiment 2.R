# 1(a) One outcome of drawing 10 coins from a chest with 20 gold, 30 silver, 50 bronze
coins <- c(rep("gold", 20), rep("silver", 30), rep("bronze", 50))
draw10 <- sample(coins, size = 10, replace = FALSE)
draw10




# 1(b) Simulate outcomes of next 10 surgical procedures (success/failure)
outcomes <- sample(c("success", "failure"),
                   size = 10,
                   replace = TRUE,
                   prob = c(0.9, 0.1))
outcomes



#Q2
samplesize = 23
sum = 0
simul = 5000

for(experiment in 1:simul){
  q = as.integer(any(duplicated(sample(365,samplesize,replace = TRUE))))
  bday = c(1:365)
  sum = sum + q
  
}
prob = sum/simul
print(prob)


#q3
conditional_probability <-function(pa,pb,pb_a){
   pa_b <- (pb_a*pa)/pb
   return(pa_b)
 }
 
 
 
 p_cloudy <-0.4
 p_rain <-0.2
 p_cloud_g_rain <-0.85
 
 conditional_probability(p_rain,p_cloudy,p_cloud_g_rain)
 

#Q4
 x = iris
 head(x, n=8)
 str(x)
 
 y = x$Sepal.Length
 y
 range(y)
 mean(y)
 quantile(y,0.25)
 quantile(y,0.75)
 
 
 
 quartiles <- quantile(iris$Sepal.Length)
 quartiles
 IQR_val <- IQR(iris$Sepal.Length)
 cat("First quartile:", quartiles[2], "\nThird quartile:", quartiles[4], "\nInterquartile Range:", IQR_val, "\n")
 
 
 
 
 IQR(y)
 sd(y)
 var(y)
 
 lapply(x[,2:4], sd)
 summary(x)



#Q5
 mode <-function(v) {
   u = unique(v)
   
   u[which.max(tabulate(match(v,u)))]
 }
 
 
 vector <- c(1,2,3,3)
 
 mode(vector)
 
 charv <- c("A","B","B","C")

mode(charv)



















