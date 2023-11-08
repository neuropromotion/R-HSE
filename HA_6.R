library(plyr)
# смотрим какие команды вообще есть
baseball$team
# загружаем в таблицу
table(baseball$team)
#отбираем тех кто чаще 200 раз встречается
above.200 = names(table(baseball$team)[table(baseball$team) >= 200])
# отображаем те строки, которые соответствуют командам которые встречаются
# чаще 200 раз:
new_baseball = baseball[baseball$team %in% above.200,]

# отбираем команды
teams = unique(baseball[baseball$team %in% above.200,]$team)
# делаем матрицу перехода
mat = matrix(NA, ncol = length(teams), nrow = length(teams))
rownames(mat) = teams
colnames(mat) = teams
mat

# заполняем циклом: идем по всей матрице, если команда одна то пропускаем, в противном случае
# записываем в x и у уникальных игроков текущих команд и в пересечении записываем 
# число игроков игравших в обеих командах
for (i in 1:length(teams)){
  for (j in 1:length(teams)){
    #if(i == j) next
    x = unique(new_baseball[new_baseball$team == teams[i],1])
    y = unique(new_baseball[new_baseball$team == teams[j],1])
    mat[teams[i], teams[j]] = length(intersect(x,y)) / min(length(x),length(y))
  }
}
mat = 1 - mat

mds = cmdscale(mat,k=2)
plot(mds, pch=19)
text(mds, rownames(mds), adj=c(1.2,1.2), col='red', cex=0.6)


# теперь сделаем вектор из размеров команд
team_size = 1:length(teams)
for (i in 1:length(teams)){
  team_size[i] = length(unique(new_baseball[new_baseball$team == teams[i],1]))
}

# теперь отрисуем все точки, размером точки будет размер команды поделенный на 100
for (i in 1:length(teams)){
  size = as.integer(team_size[i]) /150
  if (i == 1){
    plot(mds[i,1],mds[i,2], pch=19, cex=size, ylim=c(-0.4,0.4), xlim=c(-0.3,0.5))
  }
  else{
    points(mds[i,1], mds[i,2], pch=19, cex=size, ylim=c(-0.4,0.4), xlim=c(-0.3,0.5))
  }
}
text(mds, rownames(mds), adj=c(1,-.8), col='blue', cex=0.7)

