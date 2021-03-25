#
# coverageProbConfInterval02.R
#

set.seed(90125)
system.time(simulation_df <- do.call(rbind,
            lapply(1:10000,
               function(trial,p_value,sample_size,z_value){
                    successes <- sum(rbinom(sample_size,size=1,prob = p_value))
                    observed_p <- successes / sample_size
                    lower.Wald <- observed_p - z_value * sqrt(observed_p*(1-observed_p)/sample_size)
                    upper.Wald <- observed_p + z_value * sqrt(observed_p*(1-observed_p)/sample_size)
                    data.frame(trial,p_value,observed_p,z_value,lower.Wald,upper.Wald)
               },0.5,5,1.96)))

dim(simulation_df)
head(simulation_df)
table(simulation_df$observed_p)
hist(simulation_df$observed_p,main="10,000 Bernoulli Trials of N=5, p=0.5",
     xlab="Observed p-value")
# calculate coverage: % of simulations where actual p is within
# Wald confidence limits generated via simulation
sum(simulation_df$p_value > simulation_df$lower.Wald & 
         simulation_df$p_value < simulation_df$upper.Wald) / 10000 * 100
