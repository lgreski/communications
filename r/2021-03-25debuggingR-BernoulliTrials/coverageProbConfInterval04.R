#
# coverageProbConfInterval04.R
#
# corrected version of original post

f3 <- function(n,probs) {
     res1 <- lapply(n, function(i) {
          setNames(lapply(probs, function(p) {
               m<-10000
               n<-i
               p<-p
               # make number of trials m*n, and store 
               # as a matrix of 10,000 rows * n columns 
               x <- matrix(rbinom(m*n,size=1,prob = p),nrow=10000,ncol=i)
               # p.hat is simply rowSums(x) divided by n
               p.hat <- rowSums(x)/n
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