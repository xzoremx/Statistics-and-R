# Define las probabilidades p1 y p2
p1 <- 15/100
p2 <- 1

# Calcula la probabilidad de éxito en función de la cantidad de pergaminos (n)
calcular_probabilidad_exito <- function(n) {
  1 - (1 - p1 * p2)^n
}

N_Pergaminos = 8


# Crea el gráfico de línea
library(ggplot2)
ggplot(data.frame(Pergaminos = 1:N_Pergaminos), aes(x = Pergaminos, y = calcular_probabilidad_exito(Pergaminos)*100)) +
  geom_line() +
  geom_text(aes(label = paste0(round(calcular_probabilidad_exito(N_Pergaminos)*100, 2), "%")), 
            x = N_Pergaminos, 
            y = calcular_probabilidad_exito(N_Pergaminos)*100, 
            vjust = -0.5, 
            hjust = 0.5, 
            color = "blue", 
            size = 4) + # Etiqueta del último punto de éxito
  labs(x = "Cantidad de Pergaminos",
       y = "Probabilidad de Éxito (%)",
       title = "Probabilidad de Obtener un Objeto Único del Jefe") +
  theme_minimal()



