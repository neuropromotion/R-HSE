### HW ####
n = 10000
x = runif(n, min=-1, max=1)
y = runif(n, min=-1, max=1)

prob = cumsum(x^2 + y^2 <= 1)[n]/n
prob

my_pi = prob * 4
my_pi

plot(4 * cumsum(x^2 + y^2 <= 1)/1:n, xlab='toses', ylab='pi')
abline(h=pi, col='red')

##########

sex = c('m', 'm', 'f', 'f')
sex
sex1 = factor(sex)
sex1

as.factor(sex)

sex2 = factor(sex, levels=c('m', 'f', 'u'))
sex2

sex1
sex1[1]
table(sex1)
table(sex2)

sex1
table(sex1[1:3])

table(sex2)
table(as.character(sex2))

typeof(sex1)
attributes(sex1)
unclass(sex1)

d = factor(c('10', '5'))
d
as.numeric(d)
as.numeric(as.character(d))

is.factor(sex1)
class(sex1)
typeof(sex1)

########

x = list(1, 2, 'qwe')
x

x = list(c(1, 2, 4), c('qwe', TRUE))
x

x = list(abc=1:3, ltr='a', c(1, 2, 3), c(T, F))
x

str(x)
names(x)
names(x) = c('num', 'ltr', 'n', 'log')
str(x)

x$num
x$ltr

x[1]
x[3]
x[c(1, 3, 3, 2)]
x[c(T, F, F, T)]
x[c('log', 'num')]

x[1]
x[[1]]
x[['num']]
x$num

x[[1]][2]
x$num[2]

typeof(x)

#######
matrix(1, ncol=3, nrow=4)

a = matrix(1:9, ncol=3)
a
a * a
a %*% a

matrix(1:12, ncol=4)
matrix(1:12, ncol=4, byrow=T)

a = matrix(1:12, ncol=4)
length(a)
typeof(a)
dim(a) # сначала число строк, потмо число колонок
nrow(a)
ncol(a)

colnames(a) = c('a', 'b', 'c', 'd')
a

rownames(a) = c('r1', 'r2', 'r3')
a

a['r1']
a['a']
# не работает :(

attributes(a)
dimnames(a)

x = 1:10
x
dim(x) = c(2, 5)
x

x = 1:10
dim(x) = c(2, 3)
dim(x) = c(2, 6)
# не хочет

#######

a
a[1:2,] # извлек строки
a[,1:2] # извлек столбцы
a[c(1, 3), c(1, 1)]
a[c('r1', 'r3'), c('a', 'a')]

a
a[a[, 1] > 2,]

a[2,]
r = a[2,]
dim(r)
class(r)
class(a)

r = a[2,, drop=FALSE]
r
dim(r)
class(r)
class(a)

a
a > 5
which(a > 5)
which(a > 5, arr.ind=T)

#####

x = data.frame(a=1:3, b=c(T,T,F), c=c('a','b','c'))
x
cbind(a=1:3, b=c(T,T,F), c=c('a','b','c'))

rownames(x) = c('r1', 'r1', 'r3')
rownames(x) = c('r1', 'r2', 'r3')
x

colnames(x) = c('a', 'a', 'b')
x
colnames(x) = c('a', 'b', 'c')
x

dim(x)
nrow(x)
ncol(x)

x
x[2:3,]
x[,1] > 1
x[x[,1] > 1, c('b', 'a')]

typeof(x)
class(x)

x$b
x[, 'b']
x[['b']]

x[1,]
x[,1]
x[, 1, drop=FALSE]

as.matrix(x)

x$d = 0
x
x$e = 1:3
x

a
plot(a[1,])
dfa = as.data.frame(a)
dfa
plot(dfa[1,])
plot(as.numeric(dfa[1,]))

#######

x
lapply(x, typeof)
sapply(x, typeof)

l = list(1:3, F, 'a')
l

sapply(l, typeof)
sapply(l, length)
sapply(l, max)
lapply(l, max)

sapply(l, summary)
lapply(l, summary)

sapply(1:10, sqrt)

a
apply(a, 1, sum)
apply(a, 2, sum)

##########

df = data.frame(name=c('Artem', 'Kate', 'Sanya'),
                surname=c('Ar', 'Ka', 'Sa'),
                age=c(24, 21, 17),
                sex=c('m', 'f', 'f')
                )
df
df[df$sex == 'm',]
df$name
rownames(df) = df$surname
df
df['Ka',]

######

d = matrix(runif(5*24, 36, 41), nrow=5, ncol=24)
d

d[2,] > 40
which(d[2,] > 40)

colnames(d) = paste0(1:24, 'h')
d
colnames(d)[d[2,] > 40]

q = 5
paste0(q, 'h')

##########

x = runif(100, -1, 3)
x[sin(x) > 0]

m = matrix(x, ncol=10)
m

m[m[, 6] > 1,]
m[, apply(m, 2, min) < -0.5]

#######

mtcars
