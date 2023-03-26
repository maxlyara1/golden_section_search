start_point = c(-8, -3) #start point
start_step = 3 # шаг
eps = 0.00001
counter_needed <- 10000000000



jeevs = function(d, a, h, counter_needed){
  counter = 0
  e_x = c(1,0)
  e_y = c(0,1)
  f <- function(a) {
    x <- a[1]
    y <- a[2]
    counter <<- counter + 1
    10*sin(sqrt(x^2+y^2))*(cos(sqrt(x^2+y^2)))^2/(sqrt((x^2)+y^2))
  }
  graphf <- function(x, y){
    10*sin(sqrt(x^2+y^2))*(cos(sqrt(x^2+y^2)))^2/(sqrt((x^2)+y^2))
  }
  #находим значения
  graphx <- seq(-20,20,0.1)
  graphy <- seq(-20,20,0.1) 
  z <- outer(graphx,graphy,graphf)
  
  #строим графики
  contour(graphx,graphy, z, nlevels=15) 
  points(a[1],a[2],col='black', pch=20, cex=2)
  
  while(h > d & counter <= counter_needed){
    
    lr = which.max(c(f(a), f(a+e_x*h), f(a - e_x*h)))
    if(lr==2){
      a_1 = a+e_x*h
    }
    else if(lr==3){
      a_1 = a - e_x*h
    }
    else if(lr==1){
      a_1 = a
    }
    
    ud = which.max(c(f(a_1), f(a_1+e_y*h), f(a_1 - e_y*h)))
    if (ud == 1){
      a_2 = a_1
    }
    else if(ud == 2){
      a_2 = a_1 + e_y*h
    }
    else if(ud == 3){
      a_2 = a_1 - e_y*h
    }
    if (all(a_2 == a)){
      h = h/2
    }
    else {
      while (f(2*a_2-a) > f(a_2)){
        segments(a[1], a[2], a_2[1], a_2[2])
        points(a_2[1],a_2[2],col='red',pch=20, cex=2)
        a = a_2
        a_2 = 2*a_2 - a
        
      }
    }
    segments(a[1], a[2], a_2[1], a_2[2])
    points(a_2[1],a_2[2],col='green',pch=20)
    a = a_2
  }
  cat("Координаты точки экстремума:", a, "\n", "Количество вычислений целевой функции:", counter)
}
jeevs(eps, start_point, start_step, counter_needed)


#Красным отмечены точки, в которых не выполняется проверка функции во все стороны,
#а лишь изменяется точка, в ту же сторону, в которую до этого сработала максимизация функции