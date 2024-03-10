# write the initial of A
x <- c(  rep(0,500), seq(-1,-1, length.out = 1000),# first vertical line - W
         rep(0,500), seq(1,1, length.out = 1000),# second vertical line - W
         rep(0,500), seq(-1,0, length.out = 1000), # first diagonal - W
         rep(0,500), seq(0,1, length.out = 1000),
         
         rep(0,500), seq(2.5,3.5, length.out = 200),#  horizontal line - A
         rep(0,500), seq(2,3, length.out = 1000), # first diagonal - A
         rep(0,500), seq(3,4, length.out = 1000)) # second diagonal - A

y <- c( rep(0,500), seq(-1,2, length.out = 1000),# first vertical line - W
        rep(0,500), seq(-1,2, length.out = 1000),
        rep(0,500), seq(-1,1, length.out = 1000), # second diagonal - W
        rep(0,500), seq(1,-1, length.out = 1000),
        
        rep(0,500), seq(0,0, length.out = 200),# third horizontal A
        rep(0,500), seq(-1,1, length.out = 1000), # first diagonal - A
        rep(0,500), seq(1,-1, length.out = 1000)) # second diagonal - A

# matrix
z = rbind(x, y)

# intital plot
plot(y~x, xlim=c(-1,4), ylim=c(-2,4), col = "blue",
     main = "Initials")

install.packages("gifski")
library(gifski)
### Shear 
for (val in seq(-3,3, length.out=50)) {
  matrix_shear <- matrix(c(2,0, val, 1), nrow = 2, ncol = 2)
  matrix_shr <- apply(z, 2, function(x) x %*% matrix_shear)
  plot(matrix_shr[2,] ~ matrix_shr[1,], xlim= c(-6,12), ylim=c(-6,12),
       col='blue', main="Shear Method")
}
