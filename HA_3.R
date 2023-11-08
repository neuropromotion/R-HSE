# 1.

# К какому типу относится переменная mtcars:
class(mtcars) # data frame
is.data.frame(mtcars) # убеждаемся в этом
typeof(mtcars) # в действительности, data frame - это список  
is.list(mtcars) # убеждаемся в этом 

# второй столбец (вообще дата фрейм это список каждый столбец которого вектор,
# так что ответ заранее известен, убедимся):
is.vector(mtcars$cyl) # TRUE
class(mtcars$cyl) # class numeric - значения вектора числа
typeof(mtcars$cyl) # а именно типа с плавающей запятой

mtcars['Fiat 128', 'cyl'] # 4 цилиндра 
mtcars[mtcars$cyl == 4,] # выведем все машины у которых 4 цилиндра 
row.names(mtcars[mtcars$cyl == 4,]) # выведем только их названия

min(mtcars$cyl) # минимальное количество цилиндров так же 4, список выводил выше

correlation = cor(mtcars)
correlation
class(correlation) # это матрица 
is.matrix(correlation) # убеждаемся
typeof(correlation) # из значений типа double 
is.double(correlation) # убеждаемся 

names(correlation[correlation[,1] < -0.7,1]) 
# корреляция потребления бензина менее -0.7 наблюдается с количеством 
# цилиндров (cyl), со смешением (disp), кол-во лошадинных сил (hp) и весом (wt)

# 2.
vec = rnorm(100, 40, 10**2) 
vec
vec_with_each_third = vec[c(F,F,T)] # маска оставляющая каждый 3 элемент
vec_with_each_third 
vec_without_each_fifth = vec[c(T,T,T,T,F)] # маска убирающая каждый 5 элемент
vec_without_each_fifth
# теперь подвектор состоящий из элементов с четной целой частью
vec_int = sapply(vec, as.integer) # сначала делаем вектор применяя функцию
# отбрасывающую дробную часть
vec_int # получаем вектор целых чисел без дробной части
vec_even = vec[vec_int %% 2 == 0] # применяем маску для поиска четных чисел
vec_even


# 3.
# сначала сделаем листья с буквами:
first = list('a') 
second = list('b','c')
third = list('d')
fourth = list('e')

left = list(first, second) # формируем левую ветвь дерева с "а", "b" и "с"
right = list(third, fourth) # формируем правую ветвь дерева с "d", "e" 
root = list(left, right) # формируем корень дерева 

# по индексной нотации: первая цифра значит правую (1) или левую (2) ветвь
# корневого узла, вторая цифра значит правую или левую ветвь поддерева и тд
# проверяем:
root[[1]][[1]][[1]] # a
root[[1]][[2]][[1]] # b
root[[1]][[2]][[2]] # c
root[[2]][[1]][[1]] # d
root[[2]][[2]][[1]] # e
# создаем вектор и вносим туда название каждого узла:
names = c(root[[1]][[1]][[1]], root[[1]][[2]][[1]],
          root[[1]][[2]][[2]], root[[2]][[1]][[1]],
          root[[2]][[2]][[1]])
names # проверяем 
left_branch = root[[1]] # берем левое поддерево дерева 
left_branch
left_branch_right_branch = root[[1]][[2]] # берем правое поддерево левого дерева
left_branch_right_branch
