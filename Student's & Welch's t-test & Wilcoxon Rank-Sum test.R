#Ho: u1 = u2
#H1: u1 != u2

x <- rnorm(10, mean = 100, sd = 5)
y <- rnorm(20, mean = 105, sd = 5)
t.test(x,y, var.equal = TRUE)

qt(p = 0.05/2, df = 28, lower.tail = FALSE)

t.test(x,y, var.equal = FALSE)
var(x)
var(y)
dfxx <- ((36.77)**2/10 + (26.12)**2/20)**2
(36.77)**2/10 # 135.2033
135.2033**2/9 # 2031.104
(26.12)**2/20 # 34.112
34.112**2/19  # 61.243
2031.104 + 61.243 # d = 2092.347
28667.91/2092.347
df <- 13.701
wilcox.test(x,y, conf.int = TRUE)

