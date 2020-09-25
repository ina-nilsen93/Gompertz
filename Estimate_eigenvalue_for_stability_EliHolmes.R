# You need to find out if the B lies within the unit circle
# See Stability Properties of MAR(1) Models, 1st paragraph, page 306 in Ives et al 2003

#non.zero.c <- c("c11", "c13", "c14", "c15", 
#                "c22", "c24",
#                "c31", "c33", "c35",
#                "c41", "c42", "c44",
#                "c51", "c53", "c55")
#Ina values
non.zero.c <- c("c10","c11","c13","c14","c15","c16", 
                "c20","c22","c24","c26", 
                "c30","c31","c33","c35","c36",
                "c40","c41","c42","c44","c46","c47","c48","c49", 
                "c50","c51","c53","c55","c56","c57")

# just a toy set of 10 draws from the c posterior distribution
# replace with actual posterior c samples
n <- 1500
nc <- length(non.zero.c)
#posterior.c.sample <- matrix(rnorm(n*nc), n, nc)
posterior.c.sample <- m[,1:length(non.zero.c)]
# name the values to make sure they go to the right place
colnames(posterior.c.sample) <- non.zero.c


# Now create each B and get the max eigen value
B <- matrix(0, 10, 10)
vals <- c() # the max eigen values
for(i in 1:n){
  for(c in non.zero.c){
    row <- as.numeric(substr(c, 2,2))
    col <- as.numeric(substr(c, 3,3))
    row=row+1
    col=col+1
    B[row, col] <- posterior.c.sample[i,c]
  }
  # We need to find out if the B lies within the unit circle
  # To do that we take the square of the real part of the 
  # eigenvalues add to square of imaginary part
  # and take square root
  # Your B matrices shouldn't have imaginary parts I think but
  # just in case I show the full solution with real and imag parts
  lambdas <- sqrt(Re(eigen(B)$value)^2+Im(eigen(B)$value)^2)
  vals <- c(vals, max(lambdas))
}
# The vals that are > 1 are unstable (outside the unit circle)
# In my toy example, none of the vals will probably be stable,
# but that is because I randomly sampled c's



eigenvalue <- vals %>% 
        tibble::enframe(name = NULL) %>% 
        mutate(Run=1:n()) %>% 
        filter(value>1)

#save(eigenvalue, file = "eigenvalue.rdata")


ggplot(aes(x=Run, y=value), data=eigenvalue) + 
           geom_jitter() +
           geom_hline(yintercept=1, size=1, colour="red") +
           theme_minimal()


