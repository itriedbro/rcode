 size <- 12
 prob <- 1/6

 p_upto9 <- pbinom(9,size=size,prob = prob)
 p_upto9


 p_upto6 <- pbinom(6,size=size,prob = prob)
 p_upto6

 p_7to9 <- p_upto9 - p_upto6
 p_7to9


 mean_score <- 72
 sd_score <- 15.2
 score_of_interest <- 84

 cump <- pnorm(score_of_interest, mean_score, sd_score)
 p84more <- 1-cump

 perc <-p84more*100
 perc

 lambdax<-5

 p_nocar <- dpois(0,lambda = lambdax)
 p_nocar


 lambday <- 50

 p_4850 <- dpois(48,lambda = lambday) + dpois(49,lambda = lambday) + dpois(50,lambda = lambday)
 p_4850


 total_processors <- 250
 defective_processors <- 17
 sample_size <- 5
 defectives_in_sample <- 3


 prob_x_equals_3 <- dhyper(defectives_in_sample,
                           defective_processors,
                           total_processors - defective_processors,
                           sample_size)



 prob_x_equals_3


p = 0.447
n=31
x=seq(0,31,1)
pmf = dbinom(x,31,0.447)
plot(x,pmf,col = "red")
 
 pmf1 = dbinom(x,31,0.407)
 points(x,pmf1,col = "blue")



cdf <- pbinom(x,31,0.447)
plot(x,cdf,col = "green")
points(x,pmf,col = "blue")


mean_X <- n * p
variance_X <- n * p * (1 - p)
sd_X <- sqrt(variance_X)

mean_X
variance_X
sd_X

























