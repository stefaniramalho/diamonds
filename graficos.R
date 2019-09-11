# Dicionario do dataset
#https://ggplot2.tidyverse.org/reference/diamonds.html

# Carregando os módulos
library(tidyverse)
library(ggcorrplot) # Plotar as correlacoes
library(knitr) # Imprimir tabelas em um formato mais amigavel
library(kableExtra)
library(DT) # Imprimir tabelas em um formato mais amigavel
library(psych) # Funcao describe

# Criando um objento com o dataset diamonds
df <- diamonds

############# Pagina 1 #############

# Tema dos graficos
theme_dash <- theme_light() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Plot 1 - Histograma do atributo PRICE
p1 <- ggplot(df, aes(x = price)) +
  geom_histogram(bins = 40, fill = "sky blue") +
  theme_dash +
  labs(x = "Price", y = "Frequencia")


# Plot 2 - Correlacao
p2 <- df %>% select_if(is.numeric) %>%
  cor() %>%
  ggcorrplot(hc.order = TRUE,
             outline.color = "white",
             lab = TRUE,
             colors = c("red", "white", "sky blue"),
             ggtheme = ggplot2::theme_light)


# Plot 3 - Relacao entre CARAT e PRICE
p3 <- ggplot(df, aes(x = log10(carat), y = log10(price))) +
  geom_point(color = "orange", alpha = .2) +
  geom_smooth(method = lm, se = FALSE, color = "gray") +
  theme_dash +
  labs(x = "Carat", y = "Price")


# Plot 4 - Boxplot de PRICE em relacao a CUT
p4 <- ggplot(df, aes(x = cut, y = price)) +
  geom_boxplot(fill = "orange", color = "gray3") +
  theme_dash +
  labs(x = "Cut", y = "Price")

# Plot 5 - Cut
p5 <- df %>% group_by(color) %>%
  summarise(cont = n()) %>%
  ggplot(aes(x = color, y = cont)) +
  geom_bar(stat = "identity", fill = "sky blue") +
  theme_dash +
  labs(x = "Color", y = "Frequencia")

# Plot 6 - Color
p6 <- df %>% group_by(cut) %>%
  summarise(cont = n()) %>%
  ggplot(aes(x = cut, y = cont)) +
  geom_bar(stat = "identity", fill = "sky blue") +
  theme_dash +
  labs(x = "Cut", y = "Frequencia")

# Estatistica
p7 <- df %>% describe() %>%
  select(-vars, -n,) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

############# Pagina 2 #############

# Dataset (amostragem)
tabela <- df %>% sample_n(1000) %>% datatable()
