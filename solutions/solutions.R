library(tidyverse)
library(janitor)
library(naniar)
library(lubridate)


# 0 read data
temp = tempfile()
download.file('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/bicicletas-publicas/recorridos-realizados-2020.zip',
              temp)

ecobicis_df <- read_csv(unz(temp,'recorridos-realizados-2020.csv')) %>% 
  clean_names()


#1a

glimpse(ecobicis_df)
miss_var_summary(ecobicis_df)


#1b

ecobicis_df %>% 
  group_by(id_usuario) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  head(5)


#1c

ecobicis_df %>% 
  group_by(mes = month(fecha_origen_recorrido),id_usuario) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  group_by(mes) %>% 
  slice(1:5) %>% 
  group_by(id_usuario) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total))


ecobicis_df %>% 
  filter(id_usuario == 12434)

# 2

ecobicis_df %>% 
  mutate(duration = as.numeric(fecha_destino_recorrido - fecha_origen_recorrido),
         duration_diff = abs(duracion_recorrido - duration)) %>% 
  summarise(max = max(duration_diff),
            min = min(duration_diff),
            mean = mean(duration_diff),
            median = median(duration_diff),
            sd = sd(duration_diff)) %>% View()


sum(!(ecobicis_df$duracion_recorrido == as.numeric(ecobicis_df$fecha_destino_recorrido - ecobicis_df$fecha_origen_recorrido)))

# 3a

filt <- ecobicis_df$nombre_estacion_destino %>% str_detect('HOSP')

ecobicis_df %>% 
  filter(filt) %>% 
  pull(nombre_estacion_destino) %>% 
  table()


ecobicis_df %>% 
  filter(id_estacion_destino == 116) %>% 
  group_by(year = year(fecha_origen_recorrido), month = month(fecha_origen_recorrido)) %>% 
  summarise(total = n())


# 3b

ecobicis_df %>% 
  filter(id_estacion_destino == 116) %>% 
  group_by(year = year(fecha_origen_recorrido), month = month(fecha_origen_recorrido)) %>% 
  summarise(total = n()) %>% 
  mutate(total_m1 = lag(total,1),
         var_interm = (total/total_m1) -1)

# 3c

ecobicis_df %>%
  group_by(id_estacion_destino, year = year(fecha_origen_recorrido), month = month(fecha_origen_recorrido)) %>% 
  summarise(total = n()) %>% 
  mutate(total_m1 = lag(total,1),
         var_interm = (total/total_m1) -1) %>% View()

# 4

ecobicis_df %>% 
  mutate(horario = case_when(hour(fecha_destino_recorrido) %in% 6:13 ~ 'h1',
                             hour(fecha_destino_recorrido) %in% 14:21 ~ 'h2',
                             hour(fecha_destino_recorrido) %in% c(22,23,0:5) ~ 'h3',
                             T~ NA_character_)) %>%
  group_by(horario, id_estacion_destino, nombre_estacion_destino) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  group_by(horario) %>% 
  slice(1:3)
  
  
