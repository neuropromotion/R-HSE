# загружаем бибоиортеки

install.packages("tidyverse") 
install.packages('patchwork') 

# добавляем их: 

library(ggplot2)
library(dplyr)
library(patchwork) 

# загружаем тектовый файл в переменную genes 
genes = read.table("//Users//neuropromotion//Downloads//hw1.txt")

# проверка:
head(genes)


# Построим гистограммы для визуализации характеристик экспрессии кажого гена.
# Используем библиотку ggplot для построения гистограмм и patchwork для 
# объединения нескольких картинок вместе. На каждом графике черной вертикальной
# пунктирной линией отображена медиана данных:

hist_1 = ggplot(data = genes, aes(x=genes$CPY14)) + geom_histogram(binwidth = 15,
                                                                   color='black',
                                                                   fill='pink') +
  geom_vline(xintercept = median(genes$CPY14), color='black', lty=2) + 
  labs(title='CPY14', x = 'Expression', y = 'Frequency')

hist_2 = ggplot(data = genes, aes(x=genes$RMD4)) + geom_histogram(binwidth = 20,
                                                                  color='black',
                                                                  fill='pink') +
  geom_vline(xintercept = median(genes$RMD4), color='black', lty=2) +
  labs(title='RMD4', x = 'Expression', y = 'Frequency')

hist_3 = ggplot(data = genes, aes(x=genes$htVWQ)) + geom_histogram(binwidth = 25,
                                                                   color='black',
                                                                   fill='pink') +
  geom_vline(xintercept = median(genes$htVWQ), color='black', lty=2) + 
  labs(title='htVWQ', x = 'Expression', y = 'Frequency')

hist_1 + hist_2 + hist_3


# теперь отобразим графики плотности распределения. Для этого используем функцию
# melt() которая преобразит 3 таблицы в одну:

data = melt(genes) 
head(data)

ggplot(data, aes(x = value, fill = variable)) + 
  geom_density(alpha = 0.5) + xlim(-60,350) +
  ylim(0, 0.0065) + labs(title='Distribution density', x = '', y = '')


# построим линейные графики относительно двух генов, на которых будет видна 
# зависимость друг от друга:

ggplot(data = genes, aes(x = genes$CPY14, y = genes$RMD4)) + 
  geom_line(color='green') + geom_point(size=0.5) + 
  theme_dark() + labs(title='Dependency between CPY14 and RMD4 genes',
                      x = 'CPY14',
                      y = 'RMD4')

ggplot(data = genes, aes(x = genes$CPY14, y = genes$htVWQ)) + 
  geom_line(color='red') + geom_point(size=0.5) + 
  theme_dark() + labs(title='Dependency between CPY14 and htVWQ genes',
                      x = 'CPY14',
                      y = 'htVWQ')

ggplot(data = genes, aes(x = genes$RMD4, y = genes$htVWQ)) + 
  geom_line(color='blue') + geom_point(size=0.5) + 
  theme_dark() + labs(title='Dependency between RMD4 and htVWQ genes',
                      x = 'RMD4',
                      y = 'htVWQ')






# boxplot вариант первый:

boxplot_1 = ggplot(data = genes, aes(y = genes$CPY14)) + 
  geom_boxplot() + labs(title = 'CPY14')
boxplot_2 = ggplot(data = genes, aes(y = genes$RMD4)) + 
  geom_boxplot() + labs(title = 'RMD4')
boxplot_3 = ggplot(data = genes, aes(y = genes$htVWQ)) + 
  geom_boxplot() + labs(title = 'htVWQ')

boxplot_1 + boxplot_2 + boxplot_3

# ИЛИ ТАК:
ggplot(data, aes(x = '', y = value, fill = variable)) + geom_boxplot()

# осталось построить диаграммы рассеяния с уравнениями линейной регрессии:

ggplot(data = genes, aes(x = genes$CPY14, y = genes$RMD4)) + geom_point() +
  labs(title = 'Scatter plot', x = 'CPY14', y = 'RMD4') +
  geom_smooth(method = lm)

ggplot(data = genes, aes(x = genes$CPY14, y = genes$htVWQ)) + geom_point() +
  labs(title = 'Scatter plot', x = 'CPY14', y = 'htVWQ') +
  geom_smooth(method = lm)

ggplot(data = genes, aes(x = genes$htVWQ, y = genes$RMD4)) + geom_point() +
  labs(title = 'Scatter plot', x = 'htVWQ', y = 'RMD4') +
  geom_smooth(method = lm)
       

mean(genes$CPY14) #средняя экспрессия CPY14 гена

mean(genes$RMD4) #средняя экспрессия RMD4 гена

mean(genes$htVWQ) #средняя экспрессия htVWQ гена

# Оценка корреляции между средними значениями в парах генов:
t.test(genes$CPY14, genes$RMD4) # есть коррреляция
t.test(genes$CPY14, genes$htVWQ) # нет корреляции
t.test(genes$RMD4, genes$htVWQ) # есть корреляция 

# оценка корреляции между экспрессиями в парах генов:
cor.test(genes$CPY14, genes$RMD4) # p-value < 0.05 -- гены коррелируют

cor.test(genes$CPY14, genes$htVWQ) # p-value > 0.05 -- гены не коррелируют

cor.test(genes$htVWQ, genes$RMD4) # p-value > 0.05 -- гены не коррелируют 