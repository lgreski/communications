#
# coverageProbConfInt03.R
# 
# isolating problem in original code

set.seed(95014) # for reproducibility

# take parameters in original function & assign values
m<-10000
n<-5
p<-0.01
x <- rbinom(m,size=1,prob = p)

table(x) #inspect x

# calculate p value 
p.hat <- x/n
table(p.hat)

# an accurate p-value calculation
p.hat <- sum(x)/length(x)
p.hat