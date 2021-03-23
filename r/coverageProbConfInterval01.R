#
# coverageProbConfInterval01.R
#
# binomial simulation fails: original code
# 
# source: Stackoverflow.com, 2020-11-26 
# https://stackoverflow.com/questions/65028751/coverage-probability-for-confidence-interval


f3 <- function(n,probs) {
     res1 <- lapply(n, function(i) {
          setNames(lapply(probs, function(p) {
               m<-10000
               n<-i
               p<-p
               x <- rbinom(m,size=1,prob = p)
               p.hat <- x/n
               lower.Wald <- p.hat - 1.96 * sqrt(p.hat*(1-p.hat)/n)
               upper.Wald <- p.hat + 1.96 * sqrt(p.hat*(1-p.hat)/n)
               p.in.CI <- (lower.Wald <p) & ( p < upper.Wald )
               covprob1<- mean(p.in.CI)
               covprob1
          }),paste0("p=",probs))
     })
     names(res1) <- paste0("n=",n)
     res1
}
f3(n=c(10,15,20,25,30,50,100,150,200),probs = c(0.01,0.4, 0.8))