library(tidyverse)
library(janitor)
library(lubridate)

#leemos los datos
work_abs <- read_csv2('data/work_absenteeism/Absenteeism_at_work.csv') %>% clean_names()

# mini EDA
names(work_abs)
summary(work_abs)


# Seleccione todas las variables menos la variable de respuesta, el tiempo de ausencia en horas.
work_abs %>% select(-absenteeism_time_in_hours)


# Seleccione las primeras 10 variables.

work_abs %>% select(1:10)

work_abs %>% select(id, reason_for_absence, month_of_absence, day_of_the_week, 
  seasons, transportation_expense, distance_from_residence_to_work, 
  service_time, age, work_load_average_day)


# Seleccione sólo las variables que en alguno de los registros tome valores mayores que 100.

mayor_cien <- function(x){
  if(max(x)>100){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

mayor_cien_alt <- function(x){
  if_else(max(x) > 100 ,T, F)
}


work_abs %>% select_if(mayor_cien)

# Seleccione todas las observaciones de personas que sean fumadores sociales o bebedores sociales y adicionalmente pesen más de 100 kilos.

work_abs %>% 
  filter((social_drinker == 1 | social_smoker == 1) & weight > 100) 

# Identifique todos los registros en los que el peso se encuentra por encima del percentil 80 de pesos y la altura por debajo del percentil 20 de altura.

work_abs %>% 
  filter(weight > quantile(weight,0.8) & height < quantile(height,0.2)) 


# Ordene las observaciones por id y mes de ausencia

work_abs %>% 
  arrange(id, month_of_absence)

# Ordene las observaciones por educación de manera descendente y cantidad de hijos

work_abs %>% 
  arrange(desc(education), son)




# Utilice las variables age y service_time para determinar la edad de ingreso en la empresa
work_abs %>% 
  mutate(antiguedad = age - service_time) 


# El índice de masa corporal (body_mass_index) se encuentra calculado con baja precisión. Use la información disponible para calcularlo y compare las diferencias

work_abs %>% 
  mutate(bmi = weight/(height/100) ^ 2,
         bmi_diff = abs(bmi - body_mass_index))


# Identifique cuántas veces aparece cada individuo en el dataset

work_abs %>% 
  group_by(id) %>% 
  summarise(total = n())


# Para cada individuo determine además de la cantidad de veces que aparece, el total de horas de ausencia

work_abs %>% 
  group_by(id) %>% 
  summarise(total = n(),
            absenteeism_tot = sum(absenteeism_time_in_hours)
            )

# Sumemos una columna que indique para cada especie de iris cuando el largo del pétalo es muy largo (percentil 70  o más), 
# es mediano (percentil 30 o más) o corto (percentil 0 o más)

iris %>% 
  group_by(Species) %>% 
  mutate(Petal.Length_cat = case_when(Petal.Length > quantile(Petal.Length, .7) ~ 'high',
                                      Petal.Length > quantile(Petal.Length, .3) ~ 'mid',
                                      T ~ 'low'))


# Unifiquemos los atributos en del dataset de iris!

iris %>% 
  mutate(id = 1:nrow(.)) %>% 
  gather('variable', 'value', -id , -Species) 



# Volvamos a separar los atributos del dataset de iris!
  
iris %>% 
  mutate(id = 1:nrow(.)) %>% 
  gather('variable', 'value', -id , -Species) %>% 
  spread(variable, value)


#############

dates_0 <- data.frame(
  fecha_inicio = paste0(round(runif(1000,1,28)),
                        '-', round(runif(1000,1,12)),
                        '-', 2020, ' ',
                        round(runif(1000,0,23)), ':',
                        round(runif(1000,0,59)), ':',
                        round(runif(1000,0,59)))
)

write_csv(dates_0, 'data/dates_0.csv')

###############

# Importemos ‘dates_0.csv’ y hagamos un parsing correcto de timestamps

dates_0 <- read_csv('data/dates_0.csv')


# Cómo podemos crear una variable con la fecha de finalización si esta demora 1 día?

# generamos datestamp como Date y le sumamos un día

dates_0 %>% 
  mutate(fecha_inicio = dmy_hms(fecha_inicio),
         fecha_finaliz = fecha_inicio + days(1),
         fecha_finaliz2 = fecha_finaliz + hours(3),
         datestamp_finaliz = date(fecha_finaliz2))




# ejercicio lubridate


dates_0 %>% 
  mutate(fecha_fin = dmy_hms(fecha_inicio) + hours(27),
         rounded_month = month(round_date(fecha_fin, unit = 'months'))) 


dates_0 %>% 
  mutate(fecha_fin = dmy_hms(fecha_inicio) + hours(27),
         rounded_month = month(round_date(fecha_fin, unit = 'months'),label = T, abbr = F)) 



########################
noise_chars <- function(len, rl = F){
  x = c()
  if(rl){
    len <-  + rbinom(1,1,.5) - rbinom(1,1,.5)
  }
  for(i in 1:len){
    if(runif(1) > .5){
      x <- c(x, sample(letters,1))
    }
    else{
      x <- c(x, sample(1:9,1))
    }
    
    
  }
  return(x %>% paste(collapse = ''))
}

###############################


text_0 <- data.frame(
  text_0 = map_chr(1:1000, ~ paste0(letters[sample(1:26,5, T)] %>% paste(collapse = ''), 
                                    round(runif(1,1890,2021)),letters[sample(1:26,3, T)] %>% paste(collapse = ''))),
  
  text_1 = map_chr(1:1000, ~ paste0(noise_chars(5), round(runif(1,1890,2021)),noise_chars(3))),
  
  text_2 =  map_chr(1:1000, ~ paste0(noise_chars(3,rl = T), c('azul','blanco', 'rojo') %>% sample(1),noise_chars(3,rl = T)))
)

write_csv(text_0, 'data/text_0.csv')


#######################

# Importemos el dataset de text manipulation y exploremos las funciones de Stringr!
  

text_0 <- read_csv('data/text_0.csv')

# Extraigamos el año codificado en la primera columna


text_0$text_0 %>% parse_number()

# Extraigamos el año codificado en la segunda columna

text_0$text_1 %>% parse_number() # no sirve

#evaluamos largo de cada elemento
str_length(text_0$text_1)

str_length(text_0$text_1) %>% table()

#parseamos

text_0$text_1 %>% str_sub(6,9) %>% as.numeric()

# Extraigamos el color codificado en la tercera columna

text_0$text_2 %>% str_extract(pattern = regex('azul|blanco|rojo'))


# Regex

# importamos texto

regex_sample <- read_file('data/regex_sample.txt')

# Todas las palabras que proceden después de un punto (.)

str_view_all(regex_sample,'(?<=\\.[:space:])\\w+')

regex_sample %>% str_extract_all(regex('(?<=\\.[:space:])\\w+'))


# La última palabra de cada oración

str_view_all(regex_sample,'\\w+(?=\\.)')

regex_sample %>% str_extract_all(regex('\\w+(?=\\.)'))

# Todos los signos de puntuación empleados

str_view_all(regex_sample,'[:punct:]')

regex_sample %>% str_extract_all(regex('[:punct:]'))

# Las letras del alfabeto no incluídas en todo el texto 

str_view_all(regex_sample,'x')

included_letters <- regex_sample %>% 
  str_extract_all(regex('[:alpha:]')) %>% 
  pluck(1) %>% 
  str_to_lower() %>% 
  unique()


setdiff(letters, included_letters)
