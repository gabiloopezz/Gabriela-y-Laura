library(tidyverse)
data(starwars)
#Ejemplos usando select
# Seleccionar todas las columnas menos el nombre
starwars %>% select(-name)
#Seleccionar sólo las columnas que tienen subraya (_)
starwars %>% select(contains("_"))
#Seleccionar sólo las columnas que empiezan con "s"
starwars %>% select(starts_with("s"))
#Crear un data frame con los nombres y planeta de origen (homeworld)
homeworld <- starwars %>% select(name, homeworld)
#filtrar datos
#filtrar por especies: solo humanos
human <- starwars %>% filter(species == "Human")
#filtrar por especies: solo humanos del planeta Tatooine
starwars %>% filter(species == "Human", homeworld == "Tatooine")
#crear data frame con todas las especies menos los Droides
starwars_nodroids <- starwars %>% filter(species != "Droid")


#Usamos group_by y tally
starwars %>% group_by(species) %>% tally()

#Añadiendo otra variable
starwars %>% group_by(species, gender) %>% tally()

#Si lo quieres guardar en el environment recuerda asignarle un nombre
table_gender <- starwars %>% group_by(species, gender) %>% tally()

starwars %>% group_by(species) %>% summarise(mean_height = mean(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
starwars %>% group_by(species) %>% summarise(mean_height = sd(height, na.rm = T),mean_mass = mean(mass,na.rm = T))
#hacer un grafico de la altura vs. la masa de los personajes
ggplot(starwars, aes(height, mass)) + geom_point()

#Puedes modificar el color 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red")

#Modificando el color y el punto
ggplot(starwars, aes(height, mass)) + geom_point(colour = "purple", pch = 3)

#Modificando el color y el fondo 
ggplot(starwars, aes(height, mass)) + geom_point(colour = "red") + theme_light

starwars_filtrado <- starwars %>% filter(mass != 1358)
ggplot(starwars_filtrado, aes(height, mass)) + geom_point(color = "pink", size = 3)

read_csv
toy <- read_csv("seminario4/toy.csv")

resumen_media <- toy %>%
  group_by(Sex) %>%
  summarise(
    media_Weight_Kg = mean(Weight_Kg, na.rm = TRUE),
    media_Height_cm = mean(Height_cm, na.rm = TRUE),
    media_IMC = mean(IMC, na.rm = TRUE),
    media_IAS = mean(IAS, na.rm = TRUE),
    media_Ccintura = mean(Ccintura, na.rm = TRUE))

# Mostrar el resumen
print(resumen_media)

# Filtrar pacientes Women
pacientes_Women <- toy %>%
  filter(Sex == "Women")
toy %>%
  filter(Sex == "Women") %>% tally()
# Mostrar cuántos registros cumplen la condición

num_Women <- nrow(pacientes_Women)
cat("Número de pacientes femeninos:", num_Women, "\n")

# Filtrar los pacientes femeninos con sobrepeso (IMC > 25)
Women_Overweight <- pacientes_Women
 %>%
  filter(IMC > 25)

# Mostrar cuántos tienen sobrepeso
num_Overweight <- nrow(Women_Overweight)
cat("Número de pacientes Women con Overweight:", num_Overweight, "\n")

Crear gráfico de dispersión con IMC y Peso
ggplot(toy, aes(x = Weight_Kg, y = IMC)) +
  geom_point(aes(color = Sex), alpha = 0.6) + 
  labs(
    title = "Relación entre Weight_Kg e IMC",
    x = "Weigh_Kg (Kg)",
    y = "Índice de Masa Corporal (IMC)"
  ) +
  theme_minimal()



# Filtrar SampleID categorizados como "Overweight" y "Obesity"
SampleID_Overweight_Obesity <- toy %>%
  filter(IMC > 25)

# Resumen de medias agrupadas por sexo para los pacientes con sobrepeso y obesidad
resumen_media_Overweight_Obesity <- SampleID_Overweight_Obesity %>%
  group_by(Sex) %>%
  summarise(
    media_Weight_Kg = mean(Weight_Kg, na.rm = TRUE),
    media_Height_cm = mean(Height_cm, na.rm = TRUE),
    media_IMC = mean(IMC, na.rm = TRUE),
    media_IAS = mean(IAS, na.rm = TRUE),
    media_Ccintura = mean(Ccintura, na.rm = TRUE)
  )

Obesity <- toy %>% filter(IMC_clas != "Normal")

# Gráfico de dispersión IMC vs Peso para los pacientes con sobrepeso y obesidad
ggplot(Obesity, aes(x = Weight_Kg, y = IMC)) +
  geom_point(aes(color = Sex), alpha = 0.6) + 
  labs(
    title = "Relación entre Weight_Kg e IMC (Overweight y Obesidad)",
    x = "Weight_Kg (Kg)",
    y = "Índice de Masa Corporal (IMC)"
  ) +
  theme_minimal()

install.packages("ape")
install.packages("phangorn")
install.packages("phytools")    
