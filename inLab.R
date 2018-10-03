3 + 5

x <- 5
y <- 6

x + y

z <- x + y

#vectors
a <- c(1,2,3)
b <- c(10, 11, 12)

a + b

c <- c(100, 200)
a + c # c(1,2,3) + c(100, 200, 100)

a*c
a*b
c/b

a[1]
a[2]
a[c(1,2)]
a[1:3]

a[-1]
v <- c(1,2,3,4,5,6,7,8)
v[-c(1,3,8)]
v[-c(1:3)]
v[8:1]


s <- "asd"
p <- c("asd", "klm", "xyz")

mode(s)
mode(p)
class(s)
class(p)
typeof(p)
typeof(s)


mode(a)
typeof(a)
class(a)


p[1]

q  <- 'aaa'
q

setwd("C:/Users/Furkan/Desktop/lab_")

df.auto <- read.table("autompg1.csv", header = TRUE, sep = ",", dec = ".")
mode(df.auto)
class(df.auto)
typeof(df.auto)

is.data.frame(df.auto)
is.numeric(df.auto)
is.numeric(df.auto$mpg)

is.factor(df.auto$origin)
is.factor(df.auto$modelyear)

df.auto$origin <- factor(df.auto$origin, levels = c("1", "2", "3"), 
                         labels = c("US", "Europe", "Japan"))

is.numeric(df.auto$origin)
is.factor(df.auto$origin)
is.ordered(df.auto$origin)

df.auto$origin <- ordered(df.auto$origin, levels = c("Japan", "US", "Europe"))
df.auto$origin 
df.auto$origin <- factor(df.auto$origin, ordered = FALSE)
df.auto$origin
is.ordered(df.auto$origin)

levels(df.auto$origin)


df.auto$accel1 <- cut(df.auto$acceleration, 3, labels=c("fast", "medium", "slow"))
summary(df.auto$acceleration)

df.auto$accel2 <- cut(df.auto$acceleration, breaks=c(min(df.auto$acceleration)-1,10, 15, 20,max(df.auto$acceleration))
                    , labels = c("faster", "fast", "slow","slower"))

summary(df.auto$accel2)
is.factor(df.auto$accel2)
is.ordered(df.auto$accel2)
df.auto$accel2 <- ordered(df.auto$accel2, c("slower", "slow", "fast", "faster") )
is.ordered(df.auto$accel2)
df.auto$accel2
levels(df.auto$accel2)


table(df.auto$accel2, df.auto$origin)
qqplot(df.auto$mpg, df.auto$horsepower)

mean(df.auto$cylinders)
sd(df.auto$cylinders)
var(df.auto$cylinders)
median(df.auto$cylinders)

table(df.auto$cylinders)

sort(table(df.auto$cylinders), decreasing=TRUE)[1] #finding the mode



df.auto$acceleration

df.auto
edit(df.auto)

names(df.auto)
head(df.auto)
tail(df.auto)

summary(df.auto)

df.auto$mpg

df.auto[1,]
df.auto[,1]
df.auto[1,1]
df.auto[1:3, 4:6]
df.auto[c(1, 100, 200, 300), c(1,3,5)]
df.auto[,-1]

summary(df.auto$horsepower)
summary(df.auto$origin)

mean(df.auto$mpg)
sd(df.auto$mpg)
quantile(df.auto$mpg, c(0.1, 0.4, 0.8))


hist(df.auto$mpg)
plot(df.auto$mpg)
boxplot(df.auto$mpg)
boxplot(df.auto$mpg, df.auto$horsepower)

plot(df.auto$mpg, df.auto$weight)
cor(df.auto$mpg, df.auto$weight)

corMW <- cor(df.auto$mpg, df.auto$weight)
corMW

corMW*corMW

read.table(url("https://raw.githubusercontent.com/furkangursoy/MIS542-Fall18/master/autompg1.csv"))





