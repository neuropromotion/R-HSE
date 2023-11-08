install.packages('ape')
library('ape')
# читаем
x = read.dna(file = 'C:\\Users\\neuropromotion\\Desktop\\R\\4\\sequence.fasta', format='fasta', as.character = TRUE)
new = cbind(x)
# получаем вектор из букв
dim(new) = c(5132068)
# получаем вектор-строку единичной длины 
string = paste(new, collapse = '')

#получаем всевозможные k-меры :
by = nchar(string) - 5 # левый счетчик должен идти с 1 и недоходить до конца
# на 6 шагов, правый наоборот, с 6 и до конца
# получаем к-меры собственно:
k.mers = substr(rep(string, nchar(string)), 1:by, 6:nchar(string))
# используем таблицу что бы получить 4096 к-меров и их частоту 
k.mers_table = table(k.mers)

# ищем самый редкий и самый частый 
min(k.mers_table) # 24
max(k.mers_table) # 5883

# ищем 2-й самый редкий и 2-й самый частый:
min(k.mers_table[k.mers_table!=24]) # 25
max(k.mers_table[k.mers_table!=5883]) # 5666

#выводим:
print(paste('Самым редким 6-мером оказался: ', names(k.mers_table[k.mers_table==24]),
            ', который встретился ', k.mers_table[k.mers_table==24], 'раза в геноме'))
print(paste('2-м самым редким 6-мером оказался: ', names(k.mers_table[k.mers_table==25]),
            ', который встретился ', k.mers_table[k.mers_table==25], 'раза в геноме'))
print(paste('Самым частым 6-мером оказалося: ', names(k.mers_table[k.mers_table==5883]),
            ', который встретился ', k.mers_table[k.mers_table==5883], 'раза в геноме'))
print(paste('2-м самым частым 6-мером оказалося: ', names(k.mers_table[k.mers_table==5666]),
            ', который встретился ', k.mers_table[k.mers_table==5666], 'раза в геноме'))
