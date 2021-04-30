library(shiny)
library(shinydashboard)
library(shinyWidgets)

#pacotes do tidyverse
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)

#dados ajustado
dta.sen <- read.csv(
  '/home/matheus/Documentos/UNB/Laboratório 1 - Shiny/Projeto/Scripts/Ajuste dos dados.csv',
  header = T,sep = ',')

#perguntas do questionário
democracia <- c('Em geral, qual o seu nível de satisfação com a Democracia no Brasil?',
                'Como você avalia o trabalho do Senado para atender às necessidades da população?',
                'Na sua opinião, o quanto o Senado Federal e a Câmara dos Deputados são importantes para a fiscalização do governo federal?',
                'Como você avalia seu interesse por política?',
                'Com que frequência você acompanha notícias sobre o que está sendo debatido no Senado',
                'Na política se fala em esquerda, direita e centro. Você se considera mais de: ')

fatores.voto <- c("Você votou nas eleições municipais?",
                  'Na eleição para prefeito, o que foi mais importante para a decisão do seu voto?',
                  'E na eleição para vereador, o que foi mais importante para a decisão do seu voto?')

auxilios.econ <- c('Na sua opinião, programas de auxílio financeiro do governo para pessoas de baixa renda devem existir:',
                   'As pessoas procuram ajuda financeira do governo porque não querem trabalhar')

eleicao.voto <- c('Você confia no resultado das urnas eletrônicas em eleições',
                  'Algumas pessoas só votam em determinados partidos porque têm menos estudo',
                  'Em algumas situações, o voto de pessoas como você deveria valer mais do que o voto de outras pessoas')

politica <- c('Na hora de fazer leis, os políticos devem levar em conta o que diz a tradição das religiões')

seguranca <- c('Antes de ser punido, um criminoso deve ter o direito de se defender na justiça',
               'Facilitar a posse de armas vai aumentar a segurança no Brasil',
               'Deveria existir pena de morte no Brasil')

preconceito <- c('No Brasil, homossexuais sofrem muita discriminação')

raca <- c('O sistema de cotas para negros em universidade é justo')

mulher <- c('As mulheres devem ter o direito de interromper a gravidez com segurança, caso elas queiram')

meio.ambiente <- c('De forma geral, a natureza e o meio-ambiente são bem protegidos no Brasil',
                   'O aquecimento global é preocupante')

maconha <- c('A maconha para uso medicinal deve ser legalizada',
             'A maconha para uso recreativo de ser legalizada')

perguntas <- list(democracia,fatores.voto,auxilios.econ,eleicao.voto,politica,seguranca,
                  preconceito,raca,mulher,meio.ambiente,maconha)

#
ui <- dashboardPage(
  dashboardHeader(title = "DataSenado"),
  
  dashboardSidebar(
    width = 285,
    sidebarMenu(id = "menu",
                menuItem("Perfil dos Respondentes", tabName = "op-1" ,icon = icon("table")
                ),
                menuItem("Democracia e Avaliação do Congresso",tabName = "op-2" ,icon = icon("table")
                ),
                menuItem("Fatores Decisivos para voto municipal",tabName = "op-3", icon = icon("table")
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
                ),
                conditionalPanel(condition = 'input.menu != "op-1" && input.menu != "about" ',
                                 selectInput('filtros1','Escolha um filtro',
                                             choices = c('Nenhum filtro selecionado','Sexo','Faixa Etária','Ocupação',
                                                         'Escolaridade','Região','Cor/Raça',
                                                         'Religião/Crença','Renda em Salários Mínimos'),
                                             selected = 'Geral')),
                conditionalPanel(condition = 'input.filtros1 != "Nenhum filtro selecionado" && input.filtros1 !=""',
                                 selectInput('filtros2','Escolha uma outro filtro',
                                             choices = c('Nenhum filtro selecionado','Sexo','Faixa Etária','Ocupação',
                                                         'Escolaridade','Região','Cor/Raça',
                                                         'Religião/Crença','Renda em Salários Mínimos'),
                                             selected = 'Nenhum filtro selecionado'))
    ),
    hr(),
    sidebarMenu(id = 'menu2', #POSSIVELMENT NÃO É O MAIS ADEQUADO PARA ESCREVER UM TEXTO.
                menuItem('Sobre',tabName = 'about',icon = icon('exclamation-circle'),
                         menuSubItem(p('ttttttttttttttttttttttttttttttttttttttttttttttttt',br(),
                                       'ttttttttttttttttttttttttttttttttttttttttttttttttt',br(),
                                       'teste',br(),
                                       'ttttttttttttttttttttttttttttttttttttttttttttttttt')))
    )
  ),
  #Separando cada Tema em uma "página" diferente:
  dashboardBody(
    tabItems(
      tabItem(tabName = "op-1",#para o perfil dos respondendes
              selectInput("perfil1","Perfil dos Respondentes de acordo com a característica selecionada abaixo",
                          choices = c('Sexo','Faixa Etária','Ocupação',
                                      'Escolaridade','Região','Cor/Raça',
                                      'Religião/Crença','Renda em Salários Mínimos')),
              fluidRow(box(plotOutput("Graf1"))),
              fluidRow(box(tableOutput("Tabela1")))
      ),
      tabItem(tabName="op-2",selectInput("pergunta2","Questionamentos em relação à Democracia e a Avaliação do Congresso",
                                         choices=c(democracia)),
              fluidRow(box(plotOutput("Graf2"))),
              fluidRow(box(tableOutput("Tabela2")))
      ),
      tabItem(tabName="op-3",selectInput("pergunta3","Questionamentos em relação aos fatores decisivos para o voto municipal",
                                         choices = fatores.voto),
              fluidRow(box(plotOutput("Graf3"))),
              fluidRow(box(tableOutput("Tabela3")))
      ),
      tabItem(tabName="op-4a",selectInput("pergunta4A","Questionamentos em relação a auxílios econômicos",
                                          choices=c(auxilios.econ)
      ),
      fluidRow(box(plotOutput("Graf4A"))),
      fluidRow(box(tableOutput("Tabela4A")))
      ),
      tabItem(tabName="op-4b",selectInput("pergunta4B","Questionamentos em relação à eleições e voto",choices = eleicao.voto),
              fluidRow(box(plotOutput("Graf4B"))),
              fluidRow(box(tableOutput("Tabela4B")))
      ),
      tabItem(tabName="op-4c",selectInput("pergunta4C","Questionamentos em relação à política",choices = politica),
              fluidRow(box(plotOutput("Graf4C"))),
              fluidRow(box(tableOutput("Tabela4C")))
      ),
      tabItem(tabName="op-4d",selectInput("pergunta4D","Questionamentos em relação à segurança pública",choices= seguranca),
              fluidRow(box(plotOutput("Graf4D"))),
              fluidRow(box(tableOutput("Tabela4D")))
      ),
      tabItem(tabName="op-4e",selectInput("pergunta4E","Questionamentos em relação a preconteito",choices = preconceito),
              fluidRow(box(plotOutput("Graf4E"))),
              fluidRow(box(tableOutput("Tabela4E")))
      ),
      tabItem(tabName="op-4f",selectInput("pergunta4F","Questionamentos em relação à raça",choices = raca),
              fluidRow(box(plotOutput("Graf4F"))),
              fluidRow(box(tableOutput("Tabela4F")))
      ),
      tabItem(tabName="op-4g",selectInput("pergunta4G","Questionamentos em relação à mulher",choices = mulher),
              fluidRow(box(plotOutput("Graf4G"))),
              fluidRow(box(tableOutput("Tabela4G")))
      ),
      tabItem(tabName="op-4h",selectInput("pergunta4H","Questionamentos em relação ao meio ambiente",choices= meio.ambiente),
              fluidRow(box(plotOutput("Graf4H"))),
              fluidRow(box(tableOutput("Tabela4H")))
      ),
      tabItem(tabName="op-4i",selectInput("pergunta4I","Questionamentos em relação à legalização da maconha",choices = maconha),
              fluidRow(box(plotOutput("Graf4I"))),
              fluidRow(box(tableOutput("Tabela4I")))
              
      )
    )  
  )
)


server <- function(input, output) {
  
  df.senado <- reactive({
    dta.sen
  })
  
  ## Gráficos e Tabelas (servidor)
  #Gráfico Tema 1 (Perfil dos Respondentes)
  output$Graf1 <- renderPlot({
    if(input$perfil1 == 'Sexo'){
      pie(table(df.senado()$V05))
    }else if(input$perfil1 == 'Faixa Etária'){
      barplot(table(df.senado()$V05))
    } else if(input$perfil1 == 'Ocupação'){
      barplot(table(df.senado()$V10_A))
    } else if(input$perfil1 == 'Escolaridade'){
      barplot(table(df.senado()$V08))
    } else if(input$perfil1 == 'Região'){
      barplot(table(df.senado()$P01_A))
    } else if(input$perfil1 == 'Cor/Raça'){
      barplot(table(df.senado()$V07))
    }else if(input$perfil1 == 'Religião/Crença'){
      barplot(table(df.senado()$V11))
    } else if (input$perfil1 =='Renda em Salários Mínimos'){
      barplot(table(df.senado()$V12))
    }
  })
  
  output$Tabela1 <- renderTable({
    if(input$perfil1 == 'Sexo'){
      table(df.senado()$V05)
    }else if(input$perfil1 == 'Faixa Etária'){
      table(df.senado()$V05)
    } else if(input$perfil1 == 'Ocupação'){
      table(df.senado()$V10_A)
    } else if(input$perfil1 == 'Escolaridade'){
      table(df.senado()$V08)
    } else if(input$perfil1 == 'Região'){
      table(df.senado()$P01_A)
    } else if(input$perfil1 == 'Cor/Raça'){
      table(df.senado()$V07)
    }else if(input$perfil1 == 'Religião/Crença'){
      table(df.senado()$V11)
    } else if (input$perfil1 =='Renda em Salários Mínimos'){
      table(df.senado()$V12)
    }
  })
  
  
  #Gráfico Tema 2 (Democracia e Avaliação do Congresso)
  output$Graf2 <- renderPlot({
    ###PERGUNTA 1 DE DEMOCRACIA
    if(input$pergunta2 == democracia[1]){
      if(input$filtros1 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V05))
      } else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
        
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
        
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Região' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }
    } 
    ###PERGUNTA 2 DE DEMOCRACIA
    if(input$pergunta2 == democracia[2]){
      if(input$filtros1 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V05))
      } else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Sexo' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
        
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Faixa Etária' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Ocupação' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
        
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Escolaridade' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Região' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Região' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Cor/Raça' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Religião/Crença' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Nenhum filtro selecionado'){
        pie(table(df.senado()$V06))
      } else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Sexo'){
        pie(table(df.senado()$V07))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Faixa Etária'){
        pie(table(df.senado()$V08))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Ocupação'){
        pie(table(df.senado()$V09))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Escolaridade'){
        pie(table(df.senado()$V10))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Região'){
        pie(table(df.senado()$V11))
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Cor/Raça'){
        pie(table(df.senado()$V05),col = 8)
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Religião/Crença'){
        pie(table(df.senado()$V05),col = 10)
      }else if(input$filtros1 == 'Renda em Salários Mínimos' & input$filtros2 == 'Renda em Salários Mínimos'){
        pie(table(df.senado()$V06),col = 1)
        
      }
    }
    
  })
  
  output$Tabela2 <- renderTable({
    df
  })
  
  
  
  #Gráfico Tema 3 (Fatores Decisivos para voto municipal)
  output$Graf3 <- renderPlot({
    df
  })
  
  #Gráfico Tema 4 (Perfil Psicométrico)
  output$Graf4A <- renderPlot({
    df
  })
}

shinyApp(ui, server)




