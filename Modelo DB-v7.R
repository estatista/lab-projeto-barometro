library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(expss)
library(plotly)
library(survey)
library(dplyr)
library(purrr)
library(DT)
library(RColorBrewer)

dta.sen <- read.csv(
  'Dados_Ajustados.csv',
  header = T,sep = ',')


#############
# DASHBOARD #
#############

ui <- dashboardPage(
  dashboardHeader(title = "DataSenado"),
  
  dashboardSidebar(
    tags$style(".main-header .navbar {
                    margin-left: 300px;
                }
                
                .main-header .logo {
                    width: 300px;
                }"),
    sidebarMenu(id = "menu",
                menuItem("Perfil dos Respondentes", tabName = "op-1" ,icon = icon("table")
                ),
                menuItem("Democracia e Avaliação do Congresso",tabName= "op-2" ,icon = icon("table")
                ),
                menuItem("Fatores Decisivos para voto municipal",tabName= "op-3", icon = icon("table")
                ),
                menuItem("Perfil Psicométrico", icon = icon("table"),
                    menuSubItem("Auxílios Econômicos", tabName = "op-4a",icon = icon("angle-right")),
                    menuSubItem("Eleições e voto", tabName = "op-4b",icon = icon("angle-right")),
                    menuSubItem("Política", tabName = "op-4c", icon = icon("angle-right")),
                    menuSubItem("Segurança Pública", tabName = "op-4d", icon = icon("angle-right")),
                    menuSubItem("Preconceito", tabName = "op-4e", icon = icon("angle-right")),
                    menuSubItem("Raça", tabName = "op-4f", icon = icon("angle-right")),
                    menuSubItem("Mulher", tabName = "op-4g", icon = icon("angle-right")),
                    menuSubItem("Meio Ambiente", tabName = "op-4h", icon = icon("angle-right")),
                    menuSubItem("Legalização da Maconha", tabName = "op-4i", icon = icon("angle-right"))
                )
    ),
    hr(),
    selectInput('filtros1','Escolha um filtro',
                choices = c('Nenhum filtro selecionado','Sexo'='V05','Faixa Etária'='V06','Força de Trabalho'='V10','Ocupação' = 'V10_A',
                            'Escolaridade'='V08','Região'='P01_A','Cor/Raça' = 'V07',
                            'Religião/Crença' = 'V11','Renda em Salários Mínimos'= 'V12')),
    
    conditionalPanel(condition= 'input.filtros1 != "Nenhum filtro selecionado"',
                    selectInput('filtros2','Escolha outro filtro',
                                choices = c('Nenhum filtro selecionado','Sexo'='V05','Faixa Etária'='V06','Força de Trabalho'='V10','Ocupação' = 'V10_A',
                                            'Escolaridade'='V08','Região'='P01_A','Cor/Raça' = 'V07',
                                            'Religião/Crença' = 'V11','Renda em Salários Mínimos'= 'V12'))),
    hr(),
    sidebarMenu(id = 'menu2',
                menuItem('Sobre',icon = icon('exclamation-circle'),href="https://www12.senado.leg.br/institucional/datasenado/arquivos/para-brasileiros-o-senado-e-a-camara-sao-muito-importantes-para-a-democracia-1")),
    width = 300
    ),
  # Separando cada Tema em uma "página" diferente:
  dashboardBody(
    tabItems(
      tabItem(tabName="op-1",selectInput("pergunta1","Escolha uma pergunta:",choices=c('Em que estado você mora?' = 'P01',
                                                                  'Em relação à sua vida, nos últimos 6 meses, você diria que sua sensação de bem-estar:'='P02',
                                                                  'Qual a sua maior preocupação hoje?' ='P03',
                                                                  'Segue algum senador nas redes sociais?'='P07_2',
                                                                  
                                                                  'Sexo'='V05','Faixa Etária'='V06','Força de Trabalho'='V10','Ocupação' = 'V10_A',
                                                                  'Escolaridade'='V08','Região'='P01_A','Cor/Raça' = 'V07',
                                                                  'Religião/Crença' = 'V11','Renda em Salários Mínimos'= 'V12'
                                                                  )),
              fluidRow(box(plotlyOutput("Graf1"),width=9)),
              fluidRow(box(dataTableOutput("Tabela1")))
      ),
      tabItem(tabName="op-2",selectInput("pergunta2","Escolha uma pergunta:",choices=c("Avaliação do trabalho do Senado para atender as necessidades da população" =  "P08"
                                                                  , 'Importância do Senado Federal e da Câmara dos Deputados para a democracia' = 'P09',
                                                                  'Importância do Senado Federal e da Câmara dos Deputados para fiscalização do governo federal' ='P10',
                                                                  'Qual frase melhor descreve sua opinião?' ='P11',
                                                                  'Nível de satisfação com a democracia no Brasil' = 'P12')),
              fluidRow(box(plotlyOutput("Graf2"),width=9)),
              fluidRow(box(tableOutput("Tabela2")))
      ),
      tabItem(tabName="op-3",selectInput("pergunta3","Escolha uma pergunta:",choices=c("Na eleição para prefeito, o que foi mais importante para a decisão do seu voto?"=  "P15",
                                                                  "E na eleição para vereador, o que foi mais importante para a decisão do seu voto?"='P15_A')),
              fluidRow(box(plotlyOutput("Graf3"),width=9)),
              fluidRow(box(tableOutput("Tabela3")))
      ),
      tabItem(tabName="op-4a",selectInput("pergunta4A","Escolha uma pergunta:",choices=c("As pessoas procuram ajuda financeira do governo porque não querem trabalhar" ="P18_5",
                                                                    'O quanto você sabe sobre o auxílio emergencial aprovado pelo Congresso Nacional para trabalhadores informais, autônomos e desempregados?' ='P19',
                                                                    'Na sua opinião, programas de auxílio financeiro do governo para pessoas de baixa renda devem existir' = 'P20')),
              fluidRow(box(plotlyOutput("Graf4A"),width=9)),
              fluidRow(box(tableOutput("Tabela4A")))
      ),
      tabItem(tabName="op-4b",selectInput("pergunta4B","Escolha uma pergunta:",choices=c("Lembra em quem votou para senador na última eleição?" = "P07_1",
                                                                                         "Em que estado você vota?"  = 'P13',
                                                                                         "Você votou nas eleições municipais?"  ='P14',
                                                                                         "Em algumas situações, o voto de pessoas como você deveria valer mais do que o voto de outras pessoas."  ='P16_1',
                                                                                         "Algumas pessoas só votam em determinados partidos porque têm menos estudo." = 'P16_2',
                                                                                         "Você confia no resultado das urnas eletrônicas em eleições."  = 'P16_4')),
              fluidRow(box(plotlyOutput("Graf4B"),width=9)),
              fluidRow(box(tableOutput("Tabela4B")))
      ),
      tabItem(tabName="op-4c",selectInput("pergunta4C","Escolha uma pergunta:",choices=c("Na política se fala em esquerda, direita e centro. Você se considera mais de:" = 'P21',
                                                                                         "Como você avalia seu interesse por política?"  = "P04",
                                                                                         "Qual a principal forma de comunicação que você utiliza para se informar sobre o política?"  = 'P05',
                                                                                         "Com que frequência você acompanha notícias sobre o que está sendo debatido no Senado:" = 'P06',
                                                                                         "Na hora de fazer leis, os políticos devem levar em conta o que diz a tradição das religiões"  = 'P18_6')),
              fluidRow(box(plotlyOutput("Graf4C"),width=9)),
              fluidRow(box(tableOutput("Tabela4C")))
      ),
      tabItem(tabName="op-4d",selectInput("pergunta4D","Escolha uma pergunta:",choices=c("Antes de ser punido, um criminoso deve ter o direito de se defender na justiça." = "P16_3",
                                                                                         "Deveria existir pena de morte no Brasil."='P18_1',
                                                                                         "Facilitar a posse de armas vai aumentar a segurança no Brasil."='P18_2')),
              fluidRow(box(plotlyOutput("Graf4D"),width=9)),
              fluidRow(box(tableOutput("Tabela4D")))
      ),
      tabItem(tabName="op-4e",selectInput("pergunta4E","Escolha uma pergunta:",choices=c("No Brasil, homossexuais sofrem muita discriminação."="P17_2")),
              fluidRow(box(plotlyOutput("Graf4E"),width=9)),
              fluidRow(box(tableOutput("Tabela4E")))
      ),
      tabItem(tabName="op-4f",selectInput("pergunta4F","Escolha uma pergunta:",choices=c("O sistema de cotas para negros em universidades é justo."="P17_1")),
              fluidRow(box(plotlyOutput("Graf4F"),width=9)),
              fluidRow(box(tableOutput("Tabela4F")))
      ),
      tabItem(tabName="op-4g",selectInput("pergunta4G","Escolha uma pergunta:",choices=c("As mulheres devem ter o direito de interromper a gravidez com segurança, caso elas queiram"="P17_3")),
              fluidRow(box(plotlyOutput("Graf4G"),width=9)),
              fluidRow(box(tableOutput("Tabela4G")))
      ),
      tabItem(tabName="op-4h",selectInput("pergunta4H","Escolha uma pergunta:",choices=c("De forma geral, a natureza e o meio-ambiente são bem protegidos no Brasil"="P17_4",
                                                                                         "O aquecimento global é um problema preocupante"='P17_5')),
              fluidRow(box(plotlyOutput("Graf4H"),width=9)),
              fluidRow(box(tableOutput("Tabela4H")))
      ),
      tabItem(tabName="op-4i",selectInput("pergunta4I","Escolha uma pergunta:",choices=c("A maconha para uso medicinal deve ser legalizada"="P18_3",
                                                                                         "A maconha para uso recreativo deve ser legalizada "='P18_4')),
              fluidRow(box(plotlyOutput("Graf4I"),width=9)),
              fluidRow(box(tableOutput("Tabela4I")))
      )   
  )  
))

server <- function(input, output) {

  ## Gráficos e Tabelas (servidor)
  
  #Gráfico Tema 1 (Perfil dos Respondentes)
  # Grafico de barras 
  
  f_graf1 <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf1 <- dta.sen %>%
        select(input$pergunta1) %>% 
        drop_na(input$pergunta1) %>% 
        ggplot(aes_string(x = input$pergunta1)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf1
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf1 <- dta.sen %>%
        select(input$pergunta1 , input$filtros1) %>% 
        drop_na(input$pergunta1) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta1)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf1
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf1 <- dta.sen %>% 
        select(input$pergunta1 , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta1) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>% 
        ggplot(aes_string(x = input$pergunta1)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        facet_wrap(~get(input$filtros2))+
        coord_flip()+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
      f_graf1
    } 
  })
  
  output$Graf1 <- renderPlotly({
    ggplotly(f_graf1())
  })
  
  
  f_tabela1 <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      pr = svymean(~as.factor(dta.sen[[input$pergunta1]]),data.rake,na.rm = T)
      tot = svytotal(~as.factor(dta.sen[[input$pergunta1]]),data.rake,na.rm = T)
      tab <- dta.sen%>%
        summarise(total = round(coef(tot),0),
                  prop = round(coef(pr),3)*100,margem = round((2*SE(pr))*100,4))%>%
        add_row(total = round(sum(coef(tot)),0),
                prop = sum(coef(pr))*100,margem = NULL)
      
      rownames(tab) <- c(levels(dta.sen[[input$pergunta1]]),'Total')
      colnames(tab) <- c('População Estimada','Percentual (%)','Margem de Erro (%)')
      tab
    }
  })
  
  output$Tabela1 <- renderDataTable({
    f_tabela1()
  })
  
  #Gráfico Tema 2 (Democracia e Avaliação do Congresso)
  
  f_graf2 <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf2 <-  dta.sen %>% 
                  select(input$pergunta2) %>% 
                  drop_na(input$pergunta2) %>%
                  ggplot(aes_string(x = input$pergunta2)) + 
                  geom_bar(fill="#A11D21") +
                  labs(x=" ", y="Frequência") +
                  theme_bw() +
                  theme(axis.title.y=element_text(colour="black", size=12),
                        axis.title.x = element_text(colour="black", size=12),
                        axis.text = element_text(colour = "black", size=9.5),
                        panel.border = element_blank(),
                        axis.line = element_line(colour = "black")) +
                  coord_flip()
                  f_graf2
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf2 <- dta.sen %>% 
        select(input$pergunta2 , input$filtros1) %>% 
        drop_na(input$pergunta2) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta2)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf2
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf2 <- dta.sen %>% 
        select(input$pergunta2 , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta2) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta2)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf2
    } 
    

  })
  
  output$Graf2 <- renderPlotly({
    ggplotly(f_graf2())
  })
  
  #Gráfico Tema 3 (Fatores Decisivos para voto municipal)
  
  f_graf3 <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf3 <- dta.sen %>% 
        select(input$pergunta3) %>% 
        drop_na(input$pergunta3) %>%
        ggplot(aes_string(x = input$pergunta3)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf3
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf3 <- dta.sen %>% 
        select(input$pergunta3 , input$filtros1) %>% 
        drop_na(input$pergunta3) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta3)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf3
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf3 <- dta.sen %>% 
        select(input$pergunta3 , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta3) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta3)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf3
    } 
    
  })
  
  output$Graf3 <- renderPlotly({
    ggplotly(f_graf3())
  })
  
  # Gráficos Tema 4 (Perfil Psicométrico)
  
  # 4A
  
  f_graf4A <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4A <- dta.sen %>% 
        select(input$pergunta4A) %>% 
        drop_na(input$pergunta4A) %>%
        ggplot(aes_string(x = input$pergunta4A)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4A
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4A <- dta.sen %>% 
        select(input$pergunta4A , input$filtros1) %>% 
        drop_na(input$pergunta4A) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4A)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4A
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4A <- dta.sen %>% 
        select(input$pergunta4A , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4A) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4A)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4A
    } 
    
  })
  
  output$Graf4A <- renderPlotly({
    ggplotly(f_graf4A())
  })
  
  # 4B
  
  
  f_graf4B <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4B <- dta.sen %>% 
        select(input$pergunta4B) %>% 
        drop_na(input$pergunta4B) %>%
        ggplot(aes_string(x = input$pergunta4B)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4B
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4B <- dta.sen %>% 
        select(input$pergunta4B , input$filtros1) %>% 
        drop_na(input$pergunta1) %>%
        drop_na(input$filtros1) %>% 
        ggplot( aes_string(x = input$pergunta4B)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4B
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4B <- dta.sen %>% 
        select(input$pergunta4B , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4B) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4B)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4B
    } 
    
  })
  
  output$Graf4B <- renderPlotly({
    ggplotly(f_graf4B())
  })
  
  
  # 4C
  
  f_graf4C <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4C <- dta.sen %>% 
        select(input$pergunta4C) %>% 
        drop_na(input$pergunta4C) %>%
        ggplot(aes_string(x = input$pergunta4C)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4C
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4C <- dta.sen %>% 
        select(input$pergunta4C , input$filtros1) %>% 
        drop_na(input$pergunta4C) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4C)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4C
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4C <- dta.sen %>% 
        select(input$pergunta4C , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4C) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4C)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4C
    } 
    
  })
  
  output$Graf4C <- renderPlotly({
    ggplotly(f_graf4C())
  })
  
  
  # 4D
  
  
  f_graf4D <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4D <- dta.sen %>% 
        select(input$pergunta4D) %>% 
        drop_na(input$pergunta4D) %>%
        ggplot(aes_string(x = input$pergunta4D)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4D
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4D <-dta.sen %>% 
        select(input$pergunta4D , input$filtros1) %>% 
        drop_na(input$pergunta4D) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4D)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4D
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4D <- dta.sen %>% 
        select(input$pergunta4D , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4D) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4D)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4D
    } 
    
  })
  
  output$Graf4D <- renderPlotly({
    ggplotly(f_graf4D())
  })
  
  # 4E
  
  f_graf4E <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4E <- dta.sen %>% 
        select(input$pergunta4E) %>% 
        drop_na(input$pergunta4E) %>%
        ggplot(aes_string(x = input$pergunta4E)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4E
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4E <- dta.sen %>% 
        select(input$pergunta4E , input$filtros1) %>% 
        drop_na(input$pergunta4E) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4E)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4E
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4E <- dta.sen %>% 
        select(input$pergunta4E , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4E) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4E)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4E
    } 
    
  })
  
  output$Graf4E <- renderPlotly({
    ggplotly(f_graf4E())
  })
  
  
  # 4F
  
  
  f_graf4F <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4F <- dta.sen %>% 
        select(input$pergunta4F) %>% 
        drop_na(input$pergunta4F) %>%
        ggplot(aes_string(x = input$pergunta4F)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4F
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4F <- dta.sen %>% 
        select(input$pergunta4F , input$filtros1) %>% 
        drop_na(input$pergunta4F) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4F)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4F
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4F <- dta.sen %>% 
        select(input$pergunta4F , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4F) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4F)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4F
    } 
    
  })
  
  output$Graf4F <- renderPlotly({
    ggplotly(f_graf4F())
  })
  
  
  # 4G
  
  
  f_graf4G <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4G <- dta.sen %>% 
        select(input$pergunta4G) %>% 
        drop_na(input$pergunta4G) %>%
        ggplot(aes_string(x = input$pergunta4G)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4G
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4G <- dta.sen %>% 
        select(input$pergunta4G , input$filtros1) %>% 
        drop_na(input$pergunta4G) %>%
        drop_na(input$filtros1) %>% 
        ggplot(aes_string(x = input$pergunta4G)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4G
    }  else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4G <- dta.sen %>% 
        select(input$pergunta4G , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4G) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4G)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4G
    } 
    
  })
  
  output$Graf4G <- renderPlotly({
    ggplotly(f_graf4G())
  })
  
  
  # 4H
  
  
  f_graf4H <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4H <- dta.sen %>% 
        select(input$pergunta4H) %>% 
        drop_na(input$pergunta4H) %>%
        ggplot(aes_string(x = input$pergunta4H)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4H
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4H <- dta.sen %>% 
        select(input$pergunta4H , input$filtros1) %>% 
        drop_na(input$pergunta4H) %>%
        drop_na(input$filtros1) %>% 
        ggplot( aes_string(x = input$pergunta4H)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4H
    }else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4H <- dta.sen %>% 
        select(input$pergunta4H , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4H) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4H)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4H
    } 
    
  })
  
  output$Graf4H <- renderPlotly({
    ggplotly(f_graf4H())
  })
  
  # 4I
  
  
  f_graf4I <- reactive({
    if(input$filtros1 == 'Nenhum filtro selecionado'){
      f_graf4I <- dta.sen %>% 
        select(input$pergunta4I) %>% 
        drop_na(input$pergunta4I) %>%
        ggplot(aes_string(x = input$pergunta4I)) + 
        geom_bar(fill="#A11D21") +
        labs(x=" ", y="Frequência") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4I
    } else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 == 'Nenhum filtro selecionado'){
      f_graf4I <- dta.sen %>% 
        select(input$pergunta4I , input$filtros1) %>% 
        drop_na(input$pergunta4I) %>%
        drop_na(input$filtros1) %>% 
        ggplot( aes_string(x = input$pergunta4I)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        theme_bw() +
        theme(axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4I
    }else if(input$filtros1 != 'Nenhum filtro selecionado' & input$filtros2 != 'Nenhum filtro selecionado'){
      f_graf4I <- dta.sen %>% 
        select(input$pergunta4I , input$filtros1, input$filtros2) %>% 
        drop_na(input$pergunta4I) %>%
        drop_na(input$filtros1) %>% 
        drop_na(input$filtros2) %>%  
        ggplot(aes_string(x = input$pergunta4I)) + 
        geom_bar(aes_string(fill=input$filtros1)) +
        scale_fill_brewer(palette="Reds")+
        labs(x=" ", y="Frequência",fill=" ") +
        facet_wrap(~get(input$filtros2))+
        theme_bw() +
        theme(panel.spacing = unit(2,"lines"),
              axis.title.y=element_text(colour="black", size=12),
              axis.title.x = element_text(colour="black", size=12),
              axis.text = element_text(colour = "black", size=9.5),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black")) +
        coord_flip()
      f_graf4I
    } 
    
  })
  
  output$Graf4I <- renderPlotly({
    ggplotly(f_graf4I())
  })
  
}

shinyApp(ui, server)





