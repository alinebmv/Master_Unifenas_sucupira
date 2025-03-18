# =====================================================
# Packages
# =====================================================
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(
  dplyr, tidyr, readr, rmarkdown, ggplot2,
  car, MASS, gtsummary, reshape2, gridExtra, purrr, tibble
)

setwd("C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira")

master <- read_delim ("Avaliacao_egressos_SemDadosSensiveis.csv", delim = ";", show_col_types = FALSE) %>% as_tibble()
colnames(master)

# ---------------------------------------------------------------------------
# Experiência prévia em ensino superior na área de medicina
# ---------------------------------------------------------------------------
exp_pre<- master[,c(4:10)]
colnames(exp_pre)<- c('A','B','C','D','E','F','G')

col<- c("Classificação", "Professor de graduação de medicina",                                                       
  "Professor de graduação de outro curso da área da saúde",                                   
  "Supervisor de estágio de graduação de medicina",                                           
  "Supervisor de estágio de graduação de outro curso da área da saúde",                       
  "Preceptor de residência médica",                                                           
  "Professor de curso de pós-graduação lato sensu (especialização, exceto residência médica)",
  "Professor de pós-graduação stricto sensu (metrado ou doutorado)")                          

exp_prev_prop<- apply(exp_pre, 2, function(x) prop.table(table(x)))
exp_prev_prop<- data.frame(exp_prev_prop)
exp_prev_prop<- exp_prev_prop[,c(1,2,4,6,8,10,12,14)]
exp_prev_prop$G.Freq<- as.numeric(c(1,0))
colnames(exp_prev_prop)<- col

exp_prev_prop_m<- melt(exp_prev_prop)
exp_prev_prop_m$Classificação <- factor(exp_prev_prop_m$Classificação, levels = c("Sim", "Não"))
custom_colors <- c("Não" = "#8DD3C7", "Sim" = "#FFFFB3")

F1<- ggplot(exp_prev_prop_m, aes(fill = factor(Classificação), x= factor(variable), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(variable ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_manual(values = custom_colors)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size =16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(5, "cm"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=c(' ', ' ', ' ', ' ',' ', ' '))+
  labs(fill = " ")


png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura1_Exp_previa.png",  
    width = 1200, height = 800)
grid.arrange(F1, ncol = 1)
dev.off()

# ---------------------------------------------------------------------------
# Você começou ou continua trabalhando com ensino superior?
# ---------------------------------------------------------------------------
exp_dur<- master[,c(11:17)]
colnames(exp_dur)<- c('A','B','C','D','E','F','G')
col_exp_dur<- c("Professor de graduação de medicina",                                                       
  "Professor de graduação de outro curso da área da saúde",                                   
  "Supervisor de estágio de graduação de medicina",                                           
  "Supervisor de estágio de graduação de outro curso da área da saúde",                       
  "Preceptor de residência médica",                                                           
  "Professor de curso de pós-graduação lato sensu (especialização, exceto residência médica)",
  "Professor de pós-graduação stricto sensu (metrado ou doutorado)")                        

exp_dur_prop<- apply(exp_dur, 2, function(x) prop.table(table(x)))
exp_dur_prop<- data.frame(exp_dur_prop)
colnames(exp_dur_prop)<- col_exp_dur
exp_dur_prop$Classificação<- c('Não', "Sim")

col_exp_dur_m<- melt(exp_dur_prop)
col_exp_dur_m$Classificação <- factor(col_exp_dur_m$Classificação, levels = c("Sim", "Não"))
custom_colors <- c("Não" = "#8DD3C7", "Sim" = "#FFFFB3")

F2<- ggplot(col_exp_dur_m, aes(fill = factor(Classificação), x= factor(variable), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(variable ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_manual(values = custom_colors)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size =16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(5, "cm"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=c(' ', ' ', ' ', ' ',' ', ' '))+
  labs(fill = " ")


png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura2_Exp_nova.png",  
    width = 1200, height = 800)
grid.arrange(F2, ncol = 1)  # Exibe os gráficos lado a lado
dev.off()

# ---------------------------------------------------------------------------
# Qual era o seu objetivo principal ao iniciar o Mestrado?
# ---------------------------------------------------------------------------
obj<- master$`Qual era o seu objetivo principal ao iniciar o Mestrado?`
obj<- data.frame(obj)
colnames(obj)<- c('obj')

obj <- obj %>%
  count(obj) %>%
  mutate(prop = n / sum(n) * 100)

F3<- ggplot(obj, aes(x = "", y = prop, fill = obj)) +
  geom_bar(stat = "identity", width = 1, color = 'white') + 
  coord_polar(theta = "y", direction = -1, clip = 'off') +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Objetivo ao fazer o mestrado")+
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 0.5, lineheight = 1.5))

png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura3_objetivo.png",  
    width = 1200, height = 500)
grid.arrange(F3, ncol = 1)  # Exibe os gráficos lado a lado
dev.off()

# ---------------------------------------------------------------------------
# Comportamento ao longo do mestrado
# ---------------------------------------------------------------------------
comport<- master[,c(19:24)]
colnames(comport)<- c('A','B','C','D','E','F')

col_comp<- c('Você manteve o interesse ao longo do curso?',
           'Você cumpriu todas as atividades propostas?',
           'Você participou das aulas com formulação de questões e sugestões para ampliação do conhecimento e crescimento do grupo?',
           'Você manteve um clima de respeito mútuo e ético em sala de aula?',
           'Seu tempo de dedicação foi suficiente para um bom desempenho?',
           'Você participou de eventos relacionados a sua linha de pesquisa?')                          

comp_prop<- apply(comport, 2, function(x) prop.table(table(x)))
comp_prop_tab <- comp_prop %>%
  map_dfr(~ tibble(Frequência = names(.x), Valor = as.numeric(.x)), .id = "Categoria") %>%
  pivot_wider(names_from = "Frequência", values_from = "Valor", values_fill = 0)
comp_prop_tab$Categoria <- col_comp
comp_prop_tab<- comp_prop_tab[,c(1,4,3,2,5,6)]


comp_prop_tab_m<- melt(comp_prop_tab)
comp_prop_tab_m$variable <- factor(comp_prop_tab_m$variable, levels = c("Sempre", "Com muita frequência",
                                                                             "Com média frequência", "Muito pouco",
                                                                             "Nunca/não"))
custom_colors_comp <- c("Sempre" = "#8DD3C7", "Com muita frequência" = "#FFFFB3",
                   "Com média frequência" = "#BEBADA","Muito pouco" ="#FB8072",
                   "Nunca/não" = "#80B1D3")

F4<- ggplot(comp_prop_tab_m, aes(fill = factor(variable), x= factor(Categoria), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(Categoria ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_manual(values = custom_colors_comp)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size =16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(5, "cm"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=c(' ', ' ', ' ', ' ',' ', ' '))+
  labs(fill = " ")

png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura4_comportamento.png",  
    width = 1200, height = 800)
grid.arrange(F4, ncol = 1)
dev.off()

# ---------------------------------------------------------------------------
# Percepção dos alunos sobre a contribuição do mestrado para sua formação profissional.
# ---------------------------------------------------------------------------
percep<- master[,c(26:33)]
colnames(percep)<- c('A','B','C','D','E','F','G', 'H')

col_percep <- c("O Curso me capacitou para atuar no ensino superior na área de saúde",                       
                "Eu me sinto capaz de planejar adequadamente atividades de ensino-aprendizagem",             
                "Eu me sinto capaz de conduzir adequadamente atividades de ensino-aprendizagem",             
                "Eu me sinto capaz de avaliar adequadamente o processo de ensino-aprendizagem",
                "Eu me sinto capaz de identificar e analisar criticamente informação científica em educação",
                "O curso modificou minha visão sobre ao processo de ensino e aprendizagem",
                "O curso modificou de minha prática como docente",
                "O curso atendeu as minhas expectativas")

percep <- percep %>% mutate_all(~replace(., . == 5, 4))

comp_percep<- apply(percep, 2, function(x) prop.table(table(x)))
comp_percep_tab <- comp_percep %>%
  map_dfr(~ tibble(Frequência = names(.x), Valor = as.numeric(.x)), .id = "Categoria") %>%
  pivot_wider(names_from = "Frequência", values_from = "Valor", values_fill = 0)
comp_percep_tab$Categoria <- col_percep

comp_percep_tab_m<- melt(comp_percep_tab)
comp_percep_tab_m$variable<- ifelse (comp_percep_tab_m$variable == '3', 'Indiferente', 'Sim')
comp_percep_tab_m$variable <- factor(comp_percep_tab_m$variable, levels = c("Indiferente", "Sim"))
custom_colors_comp <- c("Indiferente" = "#8DD3C7", "Sim" = "#FFFFB3")

F5<- ggplot(comp_percep_tab_m, aes(fill = factor(variable), x= factor(Categoria), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(Categoria ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_manual(values = custom_colors_comp)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size =16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(5, "cm"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=c(' ', ' ', ' ', ' ',' ', ' '))+
  labs(fill = " ")

png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura5_Objetivos_mestrado.png",  
    width = 1200, height = 800)
grid.arrange(F5, ncol = 1)
dev.off()

# ---------------------------------------------------------------------------
# O mestrado aumentou a empregabilidade?
# ---------------------------------------------------------------------------
empreg<- master[,43]
colnames(empreg) <- c('a')

empreg<- empreg %>%
  group_by(a) %>%
  summarise (frequencia = n()) %>%
  mutate(prop = frequencia / sum(frequencia)*100)

# Caso o mestrado tenha influenciado em sua inserção no mercado de trabalho, cite em que nível isso ocorreu
nivel<- master[,44]
colnames(nivel) <- c('a')

nivel<- nivel %>%
  group_by(a) %>%
  summarise (frequencia = n()) %>%
  mutate(prop = frequencia / sum(frequencia)*100)
nivel$b<- c('Não', rep("Sim",3))

# ---------------------------------------------------------------------------
# O mestrado aumentou salário?
# ---------------------------------------------------------------------------
colnames(master[,45])
sal<- master[,45]
colnames(sal) <- c('a')

sal<- sal %>%
  group_by(a) %>%
  summarise (frequencia = n()) %>%
  mutate(prop = frequencia / sum(frequencia)*100) %>%
  mutate(ypos = cumsum(prop) - prop/2)


## juntando bd salario e empregabilidade
junt1<- c(66.7, 33.3)
junt2<- c(61.9, 38.1)
junt<- cbind(junt1, junt2)
colnames(junt)<- c('Concluir o mestrado aumentou sua empregabilidade?', 'Aumentou meu nível salarial?')
rownames(junt)<- c('Sim', 'Não')

junt_melt<- reshape2::melt(junt)
colnames(junt_melt)<- c('resposta', 'pergunta', 'value')
junt_melt<- data.frame(junt_melt)
junt_melt$resposta <- factor(junt_melt$resposta, levels = c('Sim', 'Não'))

F6<- ggplot(junt_melt, aes(fill = factor(resposta), x= factor(pergunta), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(pergunta ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_manual(values = custom_colors)+
  #  scale_fill_grey(start=0.8, end=0.2)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size =16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(5, "cm"),
        strip.background =element_rect(fill="white"))+
  scale_x_discrete(labels=c(' ', ' ', ' ', ' ',' ', ' '))+
  labs(fill = " ")

png(file = "C:/Users/aline/OneDrive/Documentos/R/Master_Unifenas_Sucupira/Figuras/Figura6_insercao.png",  
    width = 1200, height = 800)
grid.arrange(F21, ncol = 1)
dev.off()


