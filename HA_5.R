search.n.value = function(data, n){
  if(n > length(data)){
    print('Length of vector is too less')
    return(-1)  
  }
  return(sort(data)[n])
}

plot_mandelbrot = function(xmin, xmax, ymin, ymax, size, iter){
  x0 = matrix(seq(xmin, xmax, length.out=size), nrow=size,ncol=size, byrow=TRUE)
  y0 = matrix(seq(ymin, ymax, length.out=size), nrow=size, ncol=size)
  x = x0
  y = y0
  for (times in 1:iter){
    for (i in 1:size){
      for (j in 1:size){
        temp_x = x[i,j]
        temp_y = y[i,j]
        x[i,j] = temp_x**2 - temp_y**2 + x0[i,j]
        y[i,j] = 2 * temp_x * temp_y + y0[i,j]
      }
    }
  }
  z = t(abs(x**y + y**2))
  z[!is.na(z)] = rank(z[!is.na(z)])
  image(z**3, col=c('white','black'))
}


plot_mandelbrot(-1.8,0.6,-1.09,1.09,1000,20)


