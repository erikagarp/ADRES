
# Prueba Técnica
# Erika Johanna García Pachón
# 05 de abril, 2024


#0. Instalación de Paquetes

library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(RSQLite)
library(DBI)
library(ggplot2)
library(tidyverse)
library(tidyr)

# 1. Se realiza el cargue de la base de Municipios

Municipios <- read_excel("C:/Users/USUARIO/Downloads/Municipios/Municipios.xlsx")
Municipios <- Municipios %>% 
  select(Dep,Depmun, Superficie, Poblacion, Irural, Region)

# Teniendo en cuenta que los archivos base, cuentan con errores en los nombres de municipios y 
# departamentos, se eliminan de la base inicial, conservando la información del código divipola
# que se usará como llave más adelante para la recuperación de esta información en la base.


## Se realiza el cargue de la base de datos de divipola para la corrección de los nombre de municipios y departamentos
# dentro de la base

divipola <- read_csv("C:/Users/USUARIO/OneDrive/Documentos/IGAC/2024/03. EVIDENCIAS/Cuenta 02/Fuentes/DIVIPOLA-_C_digos_municipios_20240215.csv")
divipola <- divipola %>% 
  mutate(`Nombre Departamento`=str_to_title(`Nombre Departamento`), 
         `Nombre Municipio`=str_to_title(`Nombre Municipio`)) %>% 
  select(`Código Municipio`, `Nombre Departamento`, `Nombre Municipio`) %>% 
  rename("Depmun" = `Código Municipio`, "Departamento" =`Nombre Departamento`, "Municipio" = `Nombre Municipio`)

# Se realiza el cruce por código de municipios para recuperar el nombre de departamento y municipio

Municipios <- merge(Municipios, divipola, all.x = TRUE)


# 2.  Se realiza el cargue de la base de Prestadores

Prestadores <- read.csv("C:/Users/USUARIO/Downloads/Prestadores.csv", encoding = "Latin1",
                        sep = ";")
Prestadores <- as.data.frame(Prestadores)

Prestadores <- Prestadores %>% 
  mutate(muni_nombre = str_to_title(muni_nombre), 
         Depmun = str_sub(codigo_habilitacion, 1, 5))
rm(divipola)

#3. Cargue de las bases a SQLite

# Paso 1: Establecer una conexión con la base de datos SQLite, creando la base de datos si no existe
con <- dbConnect(SQLite(), dbname = "Prueb.sqlite")

# Paso 2: Importar los dataframe, previamente revisados a SQLite, creando una nueva tabla llamada 'Municipios'

dbWriteTable(con, "Municipios", Municipios)

# Paso 3: Crear una nueva tabla llamada 'Prestadores'

dbWriteTable(con, "Prestadores", Prestadores)
rm(Municipios, Prestadores)

Municipios <- dbGetQuery(con, "SELECT * FROM Municipios")
Prestadores<- dbGetQuery(con, "SELECT * FROM Prestadores")

# 4. Análisis exploratorio de la información disponible: 
#Base Municipios

# Creando un factor con 'Dep' para tenerlo en el eje x y evitar que ggplot lo trate como un número.
Municipios$Dep <- factor(Municipios$Dep)

library(ggplot2)
library(scales)  # Para funciones de formato de etiquetas

ggplot(Municipios, aes(x=Dep, y=Region, size=Poblacion)) +
  geom_point(alpha=0.5, shape=21, colour="blue", fill="lightblue") +
  scale_size_continuous(name="Población",
                        labels = comma,  
                        range = c(2, 12)) + 
  guides(size=guide_legend(title="Población", 
                           override.aes=list(alpha=1))) +  
  labs(title="Distribución de la Población en Municipios",
       x="Código del Departamento",
       y="Región") +
  theme_minimal() +
  theme(legend.position="right",  
        legend.key.width=unit(2, "lines"),
        legend.key.height=unit(1, "lines"), 
        axis.text.x=element_text(angle=90, hjust=1))


# 4. Definción de las preguntas que se resolverán a partir del análisis de la 
# información disponible

#Pregunta 1: ¿Cuál es la distribución de prestadores de servicios por departamento?
{
prestadores_dep <- Prestadores %>%
  group_by(depa_nombre) %>%
  summarise(total_prestadores = n())

ggplot(prestadores_dep, aes(x=depa_nombre, y=total_prestadores)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# Existe una distribución desigual de prestadores de servicios entre los departamentos.
# Algunos departamentos, como Bogotá D.C., tienen una cantidad significativamente mayor de 
# prestadores de servicios comparado con otros.Bogotá D.C., que destaca con la barra más alta, 
# probablemente refleje su estatus como capital y centro urbano principal del país, donde se 
# concentran servicios médicos, educativos y otros servicios.

#  Departamentos como Amazonas muestran un número mucho menor de prestadores. Esto podría reflejar 
# una diferencia entre áreas urbanas y rurales o remotas en términos de acceso a servicios. 
# Las zonas con menor densidad de población o con menor desarrollo económico podrían tener menos 
# prestadores.

General2 <- merge(Prestadores,Municipios, by="Depmun")
General2 <- General2 %>%
  group_by(Municipio, Irural) %>%
  summarise(total_poblacion = max(Poblacion, na.rm = TRUE),
            total_prestadores = n())

ggplot(General2, aes(x=Irural, y=total_prestadores)) +
  geom_point(aes(color=Irural), alpha=0.6) +  # puntos semi-transparentes
  geom_smooth(method="lm", se=FALSE, color="blue") +  # curva de regresión lineal sin banda de confianza
  scale_color_gradient(low="blue", high="green") +  # gradiente de color de azul (urbano) a verde (rural)
  labs(title="Distribución de Prestadores de Servicios por Índice de Ruralidad",
       x="Índice de Ruralidad",
       y="Cantidad de Prestadores") +
  theme_minimal()

# Al analizar la distribución de prestadores de servicio teniendo en cuenta el índice de ruralidad, se identifica
# que  A medida que el índice de ruralidad aumenta, hay una tendencia a tener menos prestadores de servicios. Esto 
# puede reflejar la dificultad de proporcionar servicios en áreas más rurales, posiblemente debido a la logística, 
# menores poblaciones y por ende menor demanda visible.El patrón general sugiere una disminución en la cantidad de
# prestadores a medida que las áreas se vuelven más rurales, lo cual es consistente con la tendencia general 
# observada en muchas regiones y países.
rm(General2, prestadores_dep)

}

#Pregunta 2: ¿Existe una correlación entre la población de un municipio y la cantidad de prestadores de servicios?
{
General <- merge(Municipios,Prestadores, by="Depmun")
General$Poblacion <- as.numeric(as.character(General$Poblacion))

# Agrupar por Municipio, sumar la Población y contar los prestadores

General <- General %>%
  group_by(Municipio) %>%
  summarise(total_poblacion = max(Poblacion, na.rm = TRUE),
            total_prestadores = n())

# Realizar la prueba de correlación
cor.test(General$total_poblacion, General$total_prestadores)

# Generación del gráfico

ggplot(General, aes(x=total_poblacion, y=total_prestadores)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) +  
  labs(title="Correlación entre Población y Cantidad de Prestadores de Servicios",
       x="Población Total",
       y="Total de Prestadores de Servicios")

# Al analizar la gráfica, se observa una correlación alta de 0.9794 entre la 
# población total y el número total de prestadores de servicios. Además, el valor 
# de "t" es 146.8, y el p-valor es muy bajo, lo que indica una correlación estadísticamente 
# significativa.

# En respuesta a la pregunta planteada, los resultados sugieren que conforme aumenta la población 
#de un municipio, también tiende a aumentar proporcionalmente el número de prestadores de servicios. 
#Esto sugiere que la asignación de recursos a cada municipio se ajusta a sus necesidades, ya que a 
# medida que la población crece, la demanda de servicios también aumenta. En consecuencia, 
#sería prudente concluir que la asignación de recursos disponibles está en línea con las demandas 
# crecientes de la población.

punto_atipico <- General[which.max(General$total_prestadores), ]
print(punto_atipico)

# Bogotá, siendo la capital del país, tiene una población significativamente grande. Es razonable 
# esperar que una ciudad de este tamaño tenga un número alto de prestadores de servicios debido a 
# la alta demanda generada por su gran población.El número de prestadores de servicios es considerablemente 
# alto. Esto podría reflejar la centralización de servicios en la capital, donde los prestadores 
# pueden atender no solo a los residentes sino también a personas de áreas circundantes o visitantes. 
# Las capitales suelen ser centros de atención especializada y servicios de mayor complejidad, 
# lo que justificaría la alta concentración.

rm(punto_atipico, General)
  }

#Pregunta 3: ¿Cuáles son los servicios más comunes y menos comunes ofrecidos en los municipios?
{
  conteo_servicios <- Prestadores %>%
    count(clpr_nombre) %>%
    arrange(desc(n))

  # Gráfica de todos los servicios (teniendo en cuenta la frecuencia)
  ggplot(conteo_servicios, aes(x=reorder(clpr_nombre, n), y=n)) +
    geom_bar(stat="identity") +
    coord_flip() +  # Voltear las coordenadas para una mejor visualización
    labs(title="Frecuencia de Tipos de Servicios en Municipios",
         x="Tipo de Servicio",
         y="Cantidad") +
    theme_minimal()
  
  #La categoría "Profesional Independiente" domina en términos de frecuencia, lo que indica que la 
  #mayoría de los servicios registrados en los municipios pertenecen a profesionales independientes. 
  #Esto podría reflejar una alta cantidad de prestadores que operan en una capacidad privada o autónoma
  #en diversos campos como la salud, la educación, el asesoramiento legal, entre otros.
  #La siguiente categoría más común es "Instituciones Prestadoras de Servicios de Salud - IPS",esto puede 
  #deberse a que estos servicios son más especializados o tienen demandas específicas que no son tan 
  #amplias como la atención médica general o los servicios profesionales.
  
  rm(conteo_servicios)
}

#Pregunta 4: ¿Cuál es el ratio promedio de habitantes por prestador de servicios en cada región?

{
  
  General3 <- merge(Municipios,Prestadores, by="Depmun")
  General3$Poblacion <- as.numeric(as.character(General3$Poblacion))
  
  # Agrupar por Municipio, sumar la Población y contar los prestadores
  
  General3 <- General3 %>%
    group_by(Region, Municipio) %>%
    summarise(total_poblacion = max(Poblacion, na.rm = TRUE),
              total_prestadores = n()) %>% 
    mutate(Ratio_Hab = total_poblacion / total_prestadores)
  
  ratio_habitante_prestador <- General3 %>%
    group_by(Region) %>%
    summarise(Ratio_Hab = sum(Ratio_Hab) / sum(total_prestadores))
  
  # Gráfico de barras para la ratio promedio de habitantes por prestador de servicios por región
  ggplot(ratio_habitante_prestador, aes(x = Region, y = Ratio_Hab)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme_minimal() +
    labs(x = "Región",
         y = "Ratio Habitante/Prestador")

  #Al analizar la gráfica obtenida, puede sugerirse que, Las regiones con ratios más altos, 
  #como la Región Centro Sur y la Región Llano, podrían necesitar un análisis (investigación)
  #más profunda para determinar las causas de estos altos ratios y considerar estrategias 
  #para aumentar la cantidad de prestadores de servicios o mejorar la eficiencia de los servicios
  #existentes.
  
  #Estos datos pueden ser el punto de partida para una planificación estratégica dirigida a mejorar 
  #el acceso a servicios en las regiones identificadas como deficientes.
  
  rm(General3, ratio_habitante_prestador)
}

#Pregunta 5: ¿Cómo se distribuye la categoría de prestadores (por ejemplo, salud, educación, otros) 
# en relación con la población de cada municipio?
{
  
  B1 <- Prestadores %>% 
    select(Depmun, clpr_nombre) %>% 
    group_by(Depmun,clpr_nombre) %>% 
    summarise(total_prestadores = n())
  
  B2 <- merge(Municipios, B1)

  
  # Gráfico de dispersión para la relación entre la población del municipio y la distribución de categorías de servicios
  
  ggplot(B2, aes(x = Poblacion, y = clpr_nombre, size = total_prestadores)) +
    geom_point(aes(color = clpr_nombre)) +
    scale_size(range = c(1, 10)) +  # Ajusta el rango de tamaño de las burbujas
    scale_x_continuous(labels = scales::comma) +  # Formatea el eje X para evitar la notación científica
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +  # Si es necesario, envuelve el texto de los labels en el eje Y
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
         x = "Población del Municipio",
         y = "Categoría de Servicios",
         size = "Cantidad de Prestadores")
    
    #Los profesionales independientes son prominentes en casi todos los municipios y su número aumenta en 
    # municipios con poblaciones más grandes, lo cual es típico, ya que las áreas más pobladas 
    # suelen ofrecer más oportunidades y demanda para este tipo de servicios.
    
    #Las IPS están presentes en todo el espectro de poblaciones municipales, pero su número no 
    #aumenta tanto como los profesionales independientes en los municipios más poblados. 
    #Esto podría sugerir que la expansión de IPS está menos correlacionada con la población que 
    #la práctica profesional independiente.
    
    # Servicios de Objeto Social Diferente y Transporte Especial de Pacientes se distribuyen 
    #más uniformemente, sugiriendo que estos servicios pueden ser necesidades básicas que no 
    # están directamente relacionadas con el tamaño de la población de un municipio.
  
  rm(B1, B2)
}

