x <- c(58,69,64,55)
y <- c(51,41,51,71,42)
z <- c(x,y)

r <- sample(1:9,4)
r

z[r]
z[-r]

d <- c()

n <- 100000

for (i in 1:n) 
{
  r <- sample(1:9,4)
  d <- c(d,mean(z[r]) - mean(z[-r]))
}

orginal_mean <- mean(treat) - mean(control)
d[1:20]
d = as.tibble(d)
ggplot(d,aes(x=value)) + geom_histogram()
hist(d)
abline(v = orginal_mean)
p_val = sum(abs(d) >= orginal_mean) / n
p_val

library(jmuOutlier)
perm.test(x,y,stat=mean)

t.test(x,y)
wilcox.test(x,y)



treat <- c(5.8,8.3,6.2,4.3,7.7,7.2,7.1,4.8,7.4,8.3,4.1,4.4)
control <- c(5.1,4.4,3.9,4.6,5.4,4.6,5.6,5.8,2.8,4.2,4.1,3.8)

z <- c(treat,control)

r <- sample(1:24,12)
r

z[r]
z[-r]

d <- c()

n <- 200

for (i in 1:n) 
{
  r <- sample(1:24,12)
  d <- c(d,mean(z[r]) - mean(z[-r]))
}

orginal_mean <- mean(treat) - mean(control)
d
hist(d)
abline(v = orginal_mean)
p_val = sum(d >= orginal_mean) / n
p_val

library(jmuOutlier)
perm.test(treat,control,stat=mean , plot=TRUE)

t.test(treat,control)






#t.test(x,y)
#library(jmuOutlier)
#perm.test(x,y)
#?perm.test,


#z = c(x,y)
#z

#d = c()
#g1 = combn(1:9, 4)
#for(i in 1:126)
#{
#  print(z[g1[,i]])
#  print(z[-g1[,i]])
#  diff = mean(z[g1[,i]]) - mean(z[-g1[,i]])
#  d = c(d , diff)
#}
#d
#hist(d,breaks =20)
#abline(v = 10.30)
#p_val = sum(d > 10.30) / 126
#p_val
