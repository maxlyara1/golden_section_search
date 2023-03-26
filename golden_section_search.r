# Ввод
#a = as.numeric(readline(prompt = "Введите границу А: "))
#b = as.numeric(readline(prompt = "Введите границу B: "))
#epsilon = as.numeric(readline(prompt = "Введите точность: "))

# Задаём нужную функцию для золотого сечения
goldfunc = function(x){ 
  return(sin(x))
}

# Функция вывода точек минимума
min_point = function(a, b)
  points((a+b)/2, goldfunc((a+b)/2))

# Функция вывода крайних точек
bounds = function(a, b){
  points(a, goldfunc(a))
  points(b, goldfunc(b))
  }

# С помощью золотого сечения
gold = function(a, b, n){
  
  # Создаю график
  #distx <- (a+abs(a%/%3.14)):(b-abs(b)%/%3.14)
  distx <- seq(a, b, 0.1)
  disty <- goldfunc(distx)
  plot(distx, disty, type='l', xlab = "Точки", ylab = 'Значения точек',col="green",
       main="Golden-section search")
  
  fi = (1 + sqrt(5)) / 2
  x1 = b - (b - a) / fi
  x2 = a + (b - a) / fi
  m = c()
    while(b - a > n){
      # Точки границ во время цикла
      bounds(a, b)
      
      # Точки минимума во время цикла
      #min_point(a, b)
      
      if(goldfunc(x1) > goldfunc(x2)){
        a = x1
        x1 = x2
        x2 = b - (x1 - a)
        #m <- c(m, a)
      }
      else{
        b = x2
        x2 = x1
        x1 = a + (b - x2)
        #m <- c(m, b)
      }
      
      # Точки границ после выполнения цикла
      bounds(a, b)
      
      # Точки минимума после выполнения цикла
      #min_point(a, b)
    }
  
  # Вывод локального минимума и значения в этой точке
  cat("Локальный минимум в точке: ", ((a+b)/2), "\n", 
      "Значение в этой точке:", goldfunc((a+b)/2), "\n")
      #,"Значение в этой точке(2):", max(c(goldfunc(a), goldfunc(b), goldfunc(x1), goldfunc(x1))))
}


gold(-10, 10, 0.00001)
# левая граница, правая граница, эпсилон


