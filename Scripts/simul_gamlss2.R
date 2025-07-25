
n <- 100
x <- runif(n=n)
mu <- 5 + 3 * x
sigma <- sqrt(exp(-2 + 1 * x))
y <- rnorm(n=n, mean=mu, sd=sigma)
datis <- data.frame(x=x, y=y, mu=mu, sigma=sigma)

f <- y ~ x | . 
b1 <- gamlss2(f, family = NO, data = datis, K = 2)

b1
summary(b1)
plot(b1)
Rsq(b1)

new <- datis[1:5, ]
new
pre <- predict(b1, newdata=new, type="parameter")
pre
cor(pre[, 2], new$sigma)
plot(x=pre[, 2], y=new$sigma)

