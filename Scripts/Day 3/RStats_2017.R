#Exercise 1
x <- rnorm(1000,mean=10,sd=5)
length(x[x<5])/length(x)
pnorm(5,mean=10,sd=5)

#Exercises 2 and 3
1- pgamma(0.68,shape=3, scale = 1)
pexp(10,rate=0.05) - pexp(1,rate=0.05)

#Exercise 4
pbirthday(n = 15, classes = 365, coincident = 2)
pbirthday(25,687,2) #right answer for martians


#Exercise 5
darwin <- read.table('darwin.txt',header=TRUE)
m<-mean(darwin$crossfertilized-darwin$selffertilized)
s<-sd(darwin$crossfertilized-darwin$selffertilized)
t.test(darwin$crossfertilized,darwin$selffertilized,paired=TRUE)
wilcox.test(darwin$crossfertilized,darwin$selffertilized,paired=TRUE)

#Exercise 6
m<-mean(darwin$crossfertilized-darwin$selffertilized)
s<-sd(darwin$crossfertilized-darwin$selffertilized)
pwr.t.test(n=16,d=m/s,sig.level=0.05,type='paired')
pwr.t.test(d=m/s,sig.level=0.05,power=0.8,type='paired')

#Exercise 7
gala <- read.table('gala.txt',header=TRUE,row.names=1) 
gala$log.species<-log10(gala$Species)
gala$log.area<-log10(gala$Area)
gala.model<-lm(log.species~log.area, data=gala)
plot(log.species~log.area,gala)
abline(gala.model)

#Exercsie 8
gala$log.elevation <- log10(gala$Elevation)
gala.model2 <- lm(log.species~log.area+log.elevation,data=gala)
anova(gala.model,gala.model2)

#Exercise 9
gala$log.nearest <- log10(gala$Nearest)
gala$log.scruz <- log10(gala$Scruz+1)
gala$log.adjacent <- log10(gala$Adjacent)
gala.full <- lm(log.species~log.area+log.elevation+log.nearest+log.scruz +log.adjacent,gala)
gala.step <- step(gala.full,direction='backward')


ssize <- sample(0:2,40,replace=TRUE)
ssize
is.factor(ssize)
ssize.f <- factor(ssize,labels=c('s','m', 'l'))
is.factor(ssize.f)
is.ordered(ssize.f)
ssize.f <- factor(ssize,labels=c('s','m', 'l'),ordered=TRUE)
is.ordered(ssize.f)
ssize.f[41] <- 'x'
levels(ssize.f) <- c('s','m','l','x')
ssize.f[41] <- 'x'


#Exercise 10
mtcars$cyl.f <- factor(mtcars$cyl)
lm1 <- lm(mpg ~ cyl.f,mtcars)
summary(lm1) #estimates of coefficients
anova(lm1) #overall effects of cyl.f
lm2 <- lm(mpg ~ hp,mtcars)
summary(lm2)
anova(lm2) #overall effects
AIC(lm1,lm2)


#Exercise 11
rats <- read.csv('rats.csv')
g <- lm(1/time ~ poison*treat,rats)
plot(g$fitted,g$res,xlab="Fitted",ylab="Residuals",main="Reciprocal response")
qqnorm(g$res)
qqline(g$res)
anova(g)


#Exercise 12
cathedral <- read.csv('cathedral.csv')
names(cathedral)[2:3] <- c('height','length')
plot(cathedral$length,cathedral$height,type="n", xlab="Length (feet)",ylab="height (feet)")
text(cathedral$length,cathedral$height,as.character(cathedral$style))

summary(lm(height ~ length + style:length, cathedral))
summary(lm(height~length + style ,cathedral))
plot(cathedral$length,cathedral$height,type="n",xlab="length (feet)",ylab="height (feet)")
text(cathedral$length,cathedral$height,as.character(cathedral$style))
abline(34.96916,0.10058 )
abline(34.96916-8.34535,0.10058 ,lty=2)
legend('topleft',legend=c('Gothic','Romanesque'),lty=c(1,2))


#Exercise 13
binary <- read.csv('binary.csv')
binary$rank <- factor(binary$rank)
g <- glm(formula = admit ~ gre + rank + gpa, family = binomial(logit), data = binary)
drop1(g, test='Chisq')
g <- glm(formula = admit ~ rank + gpa, family = binomial(logit), data = binary)


gpa <- seq(2.26,4,length=100)
p1 <- exp(1.0521*gpa-3.4636)/(1+exp(1.0521*gpa-3.4636))
p2 <- exp(1.0521*gpa-3.4636-0.6810)/(1+exp(1.0521*gpa-3.4636-0.6810))
p3 <- exp(1.0521*gpa-3.4636-1.3919)/(1+exp(1.0521*gpa-3.4636-1.3919))
p4 <- exp(1.0521*gpa-3.4636-1.5943)/(1+exp(1.0521*gpa-3.4636-1.5943))
plot(gpa,p1,ylim=c(0,1),lty=1,type='l')
points(gpa,p2,ylim=c(0,1),lty=2,type='l')
points(gpa,p3,ylim=c(0,1),lty=3,type='l')
points(gpa,p4,ylim=c(0,1),lty=4,type='l')
legend('topright',legend=1:4,lty=1:4)

