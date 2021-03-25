#
# coverageProbConfInterval05.R
#
# optimized solution using Wald adjustment technique 

set.seed(90125)
binomialSimulation <- function(trial_size,p_value,sample_size,z_value,adjWald=FALSE){
     trials <- matrix(rbinom(trial_size * sample_size,size=1,prob = p_value),
                      nrow = trial_size,ncol = sample_size)
     observed_p <- (rowSums(trials) + (adjWald*2)) / (sample_size + (adjWald *4))
     lower.Wald <- observed_p - z_value * sqrt(observed_p*(1-observed_p)/(sample_size + (adjWald *4)))
     upper.Wald <- observed_p + z_value * sqrt(observed_p*(1-observed_p)/(sample_size + (adjWald *4)))
     coverage_pct <- sum(p_value > lower.Wald & 
                              p_value < upper.Wald) / 10000 * 100
     data.frame(sample_size,p_value,avg_observed_p=mean(observed_p),adjWald,coverage_pct)
     
}
system.time(aSimulation <- binomialSimulation(10000,0.1,5,qnorm(0.975),adjWald=FALSE))
aSimulation


# now that we have basic function working, wrap it with lapply() to 
# generate results for multiple p-values 

do.call(rbind,lapply(c(0.1,0.2,0.4,0.8),function(a,b,c,d){
     binomialSimulation(p_value = a,
                        trial_size = b,
                        sample_size = c,
                        z_value = d)
},10000,5,1.96))

# add loop for multiple sample sizes
set.seed(90125)
system.time(results <-
                 do.call(rbind, lapply(c(5, 10, 15, 20, 25, 30, 50, 100, 150, 200,300,400),
                                       function(aSample_size, p_values) {
                                            do.call(rbind, lapply(p_values, function(a, b, c, d) {
                                                 binomialSimulation(
                                                      p_value = a,
                                                      trial_size = b,
                                                      sample_size = c,
                                                      z_value = d,
                                                      adjWald =
                                                           FALSE
                                                 )
                                            }, 10000, aSample_size, qnorm(0.975)))
                                       }, c(0.01, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,.99))))

head(results,n=11)

# line plots faceted by sample size 
library(ggplot2)
p <- ggplot(data = results,aes(x = avg_observed_p, y = coverage_pct)) + geom_line()
p + facet_wrap(~sample_size) + ylim(0,100) + 
        geom_hline(yintercept = 95, color = "red") +
        xlab("Average Observed p-value") + 
        ylab("Coverage Percentage") + 
     ggtitle("Coverage Percentages by Observed p-values by Sample Size: Wald Method")

# rerun with adjusted Wald Method  

set.seed(90125)
system.time(results <-
                 do.call(rbind, lapply(c(5, 10, 15, 20, 25, 30, 50, 100, 150, 200,300,400),
                                       function(aSample_size, p_values) {
                                            do.call(rbind, lapply(p_values, function(a, b, c, d) {
                                                 binomialSimulation(
                                                      p_value = a,
                                                      trial_size = b,
                                                      sample_size = c,
                                                      z_value = d,
                                                      adjWald =
                                                           TRUE
                                                 )
                                            }, 10000, aSample_size, qnorm(0.975)))
                                       }, c(0.01, 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,.99))))

p <- ggplot(data = results,aes(x = avg_observed_p, y = coverage_pct)) + geom_line()
p + facet_wrap(~sample_size) + ylim(0,100) + 
        geom_hline(yintercept = 95, color = "red") +
        xlab("Average Observed p-value") + 
        ylab("Coverage Percentage") + 
     ggtitle("Coverage Percentages by Observed p-values by Sample Size: Adjusted Wald Method")