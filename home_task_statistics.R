### Task 1 
#########
# H0 - плотность деревьев в лесу одинаковая, разница в количестве деревьев на каждом участке - случайна
# H1 - плостность деревьев в лесу разная
observed = c(7, 12, 10, 17, 10, 13, 15)
expected = c(12, 12, 12, 12, 12, 12, 12)

x2_1 = 0

for (i in 1:length(observed)){
  x2_1 = x2_1 + ((observed[i]-expected[i])^2)/expected[i]
}

x2_1 # 5.666667
df_1 = length(observed) - 1
df_1 # 6
### p-value = ~ 0.464
### значение может чуть чуть отличаться поскольку на сайте где
### я считал р - значение я выбрать смог только значение хи-квадрат
### равное 5.65
### скрин с сайт приложу
### отклонить Н0 не можем

### Task 2 
#########

# значение 0.25 положительное, значит будет считать только правый хвост
# распределения (нормального). Поскольку значение корреляции
# низкое, но чисто "на глаз" кажется что понадобится более 20
# наблюдений что бы полчить достоверный результат (p<0.05)
# значит будем использовать формулу Z = ln((1+r)/(1-r))/2, которая 
# распределена нормально с матожиданием = 0 и дисперсией = 1/n-3,
# sd = 1/sqrt(n-3) в таком случае
# а две сигмы вправо это 1.65 * 1/sqrt(n-3)
# значит Z должно быть больше 1.65 * 1/sqrt(n-3)
z = log((1+0.25)/(1-0.25))/2
z # 0.2554128

1.65/sqrt(45-3)

# в цикле перебираем варианты пока не найдем пороговую цифру 
for (n in 4:1000){
  two_sigms = 1.65 * sqrt(1.03/(n-3))
  if (two_sigms < z){
    print(n)
    break}
  
}

# Ответ 45!

### Task 3
#########
# H0 - ожидаемое и наблюдемое количество слов ТА в геноме различаются статистически недостоверно
# H0 - слово ТА статистически достоверно встречается чаще/реже 

T = 9594
A = 8954
G = 5863
C = 5492
size = T+A+G+C
size # 29903

p_T = T/size # 0.3208374
p_A = A/size # 0.2994348
p_G = G/size # 0.1960673
p_C = C/size # 0.1836605
# проверим:
p_T + p_A + p_G + p_C # 1

observed_TA = 2377
p_TA = p_A * p_T # 0.09606989
n = size - 1 # 29902
lyambda = (size-1) * p_TA # 2872.682
sigm = sqrt(lyambda) # 53,5974

# В данном случае я использую пуассоновское распределение k успехов в n 
# экспериментах. 
# n = 29903-1 = 29902 - кол-во экспериментов (количество возможных слов длины 2)
# р = 0,09606989 - вероятность получить слово ТА. 
# Матожидание и дисперсия пуассоновского распределения равны :
# λ = n*p = 2872,682. 
# Стандартное отклонение σ = sqrt(λ) = 53,5974.
# при больших λ пуассоновское распределение не отличается от гауссовского
# по этому можем применить Z критерий:

z_3 = (lyambda - observed_TA)/sigm # 9.248242

# 9.248242 сигм это сильно больше чем две сигмы. Отклоняем Н0!


### Task 4
#########
# используем одновыборочный t-test для того что бы определить: 
# H0 - выборка извлечена из нормально распределенной генеральной совокупности с генеральный средним 61.5
# H1 - из генеральной совокупности с другим средним

temps = c(62.2,58.7,59.7,58.7,60.0,62.0,61.5,58.2,61.9,59.1,61.3,60.0,61.7,59.6,61.3,61.8,64.3,62.0,61.8)

mean_t = mean(temps) # 60.83158
sd_t = sd(temps) # 1.571995

# считаем стандартную ошибку:
se_4 = sd_t/sqrt(length(temps)) # 0.3606405

# cчитаем t 
t_4 = (mean_t - 61.5) / se_4
t_4 # -1.853428   и    1.853428 соответственно 
df_4 = length(temps) - 1
df_4
# по графику т распределения получаем р больше 0,05 - Н0 отклонить мы не можем

# подтверждаем это встроенной функцией 
t.test(temps, mu=61.5)


### Task 5
#########
# используем двухвыборочный t-test для того что бы определить: 
# H0 - две выборки из одной нормально распределенной генеральной совокупности с одним генеральный средним 
# H1 - из разных генеральных совокупностей 
first = c(2.49,1.01 ,2.81 ,2.60 ,3.09 ,4.60 ,2.50 ,1.18 ,2.20 ,3.12 ,2.90 ,3.58 ,2.89 ,4.48 ,2.17 ,1.26 ,2.63 ,1.17 ,1.72 ,1.71 ,2.45 ,3.75 ,3.31 ,3.23)
second = c(3.37 ,2.12 ,3.08 ,3.57 ,4.79 ,1.60 ,3.71 ,5.48 ,3.06 ,5.76 ,3.86 ,4.43 ,3.77 ,4.59 ,2.80 ,4.11 ,3.70 ,2.36 ,4.83 ,2.33)

# считем ошибку среднего 
se_5 = sqrt(  ((sd(first))^2/length(first))  +  ((sd(second))^2/length(second)))
se_5 # 0.3190449
 
t_5 = (mean(first)-mean(second))/se_5
t_5 # -3.282453 and 3.282453
df_5 = length(first) + length(second) - 2
df_5
# по графику (фото прикладываю) видно что р-value меньше 0.05 что позволяет нам отклонить Н0

# подтверждаем встроенной функцией
t.test(first, second)

# можно подтвердить так же тестом Манна Уитни:
wilcox.test(first, second)

