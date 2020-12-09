############################################# Entrando com a Interface pessoal ############################################ 

#Entrando com os pacotes necessários para análise

#install.packages("shiny")
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("DT")
library(DT)
#install.packages("knitr")
library(knitr)
#install.packages("plotly")
library(plotly)
#install.packages("datasets")
library(datasets)
#install.packages("leaflet")
library(leaflet)
#install.packages("colourpicker")
library(colourpicker)
#install.packages("readxl")
library(readxl)
#install.packages("tidyr")
library(tidyr)
#install.packages("openxlsx")
library(openxlsx)

#Realizando a coneção com o Shinyapp.io

#Criação da interface pessoal 

page <-  dashboardPage(skin = "blue",
                       
                       # Serve para modificar a formatação do texto no topo da página, adicionados links para sites externos e quadros de mensagens no topo da tela
                       
                       header <- dashboardHeader(title = span(
                         div(class="head",
                             #img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png",width="40",height="32", align = "center"), # Colocando o slogan ao lado do nome
                             "Desafios Educacionais", # Título do Dashboard
                             style = "font-family: Tahoma; font-weight: bold"
                         )),titleWidth = "400px",
                         
                         tags$li(a(href = 'https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb', # página externa
                                   icon("graduation-cap"),
                                   title = "Gov.br"),
                                 class = "dropdown"),
                         
                         tags$li(a(href = 'http://portal.inep.gov.br/indicadores-educacionais', # página externa
                                   icon("user-graduate"),
                                   #img(src = 'https://s1.static.brasilescola.uol.com.br/be/vestibular/-5bfe74cfc335e.jpg', # opção de colocar uma foto no icone de acesso
                                   title = "Indicadores Educacionais", height = "30px"),
                                 #style = "padding-top:10px; padding-bottom:10px;",
                                 class = "dropdown"),
                         
                         tags$li(a(href = 'http://portal.mec.gov.br/', # página externa
                                   icon("notes-medical"),
                                   title = "Mec"),
                                 class = "dropdown"),
                         
                         
                         
                         dropdownMenu(type = "message", icon = icon("exclamation-triangle"),#Criando uma mensagem de notificação
                                      messageItem(from = "Entrega parcial 1", message = "28/10/2020", href="#", icon = icon("#")), 
                                      messageItem(from = "Entrega parcial 2", message = "18/11/2020", icon = icon("#")))
                       ),
                       
                       sidebar <- dashboardSidebar( uiOutput("sidebarpanel")),
                       
                       body <- dashboardBody(tags$style(".topimg {
                                                        margin-left:-30px;
                                                        margin-right:-500px; 
                                                        margin-top:-70px;
                                                        }"), # Dimensões da imagem na tela de login
                                             
                                             tags$style(".login {
                                                        style='background-color: transparent;'
                                                        }"),
                                             
                                             tags$style(".head {
                                                        margin-left:-50px;
                                                        margin-right:-50px; 
                                                        margin-top:-5px;
                                                        }"),
                                             
                                             tags$head(
                                               tags$style(
                                                 HTML("
                                                      .skin-blue .main-sidebar .sidebar .user-panel {
                                                      background-color:transparent;
                                                      }
                                                      .skin-blue .main-sidebar .sidebar .user-panel .pull-left{
                                                      background-color:transparent;
                                                      }
                                                      ")
                                                 )
                                                 ),
                                             
                                             tags$head(tags$style(HTML(' 
                                                                       /* tabBox color text */                    
                                                                       .nav-tabs-custom > .nav-tabs > li.header {
                                                                       color: orange;
                                                                       font: normal 14pt Arial;
                                                                       text-align: justify;
                                                                       }'))), #Serve para trocar a cor do cabeçalho 
                                             
                                             tags$head(tags$style('body {color:black;font: normal 10pt Arial;text-align: justify;}')), # Trocar tamanho da fonte do texto
                                             
                                             
                                             tags$style(".nav-tabs {
                                                        background-color: #FFFFFF;height: 30px;}
                                                        
                                                        .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
                                                        background-color: transparent;
                                                        border-color: transparent;}
                                                        
                                                        .nav-tabs-custom .nav-tabs li.active {
                                                        border-top-color: #FFFFFF;
                                                        }"),   # Mudando a cor do cabeçalho das caixas de texto
                                             
                                             #   tags$head(tags$style(HTML('
                                             #                 
                                             #  /* Cor do sidebar menu*/
                                             #  .skin-blue .main-sidebar {
                                             #  background-color: rgb(255,125,125);
                                             #  }
                                             # 
                                             #  /* Cor e fonte interna do sidebar menu */
                                             #  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                             #  background-color: rgb(255,125,125);
                                             #  color: rgb(255,255,255);font-weight: bold;font-size: 18px;
                                             #  }
                                             #                             
                                             #  /* Cor da fonte externa */
                                             #  .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                             #  background-color: rgb(255,125,125);
                                             #  color: rgb(255,255,255);font-weight: bold;
                                             #  }
                                             # 
                                             # /* toggle button when hovered  */
                                             # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                             #  background-color: rgb(255,125,125);color:rgb(255,255,255);
                                             #  }'))), #Serve para manipular as cores do Menu lateral
                                             
                                             uiOutput("body"),
                                             
                                             tabItems(
                                               
                                               tabItem(tabName = "aba_2", # Adicionando as caixas de InfoBox na aba 2 
                                                       
                                                       fluidRow(
                                                         valueBoxOutput("Info1"),
                                                         valueBoxOutput("Info2"),
                                                         valueBoxOutput("Info3"),
                                                         infoBoxOutput("Info4"),
                                                         infoBoxOutput("Info5"),
                                                         infoBoxOutput("Info6"),
                                                         infoBoxOutput("Info7"),
                                                         infoBoxOutput("Info8")),
                                                       
                                                       fluidRow(
                                                         box(title = strong("Gráfico de comparação entre os anos de 2014 e 2019"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba2_1")), #Gráfico 1
                                                         box(title = strong("Gráfico de barras divergentes"), status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba2_2")), #Gráfico 2
                                                         box(title = strong("Gráfico de comparação no ano de 2014"), status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba2_3")), #Gráfico 3
                                                         box(title = strong("Gráfico de comparação no ano de 2019"), status = "warning", solidHeader = TRUE, plotlyOutput("grafico_aba2_4")) #Gráfico 4
                                                         
                                                       )),
                                               
                                               
                                               tabItem(tabName = "aba_1", # Adicionando as caixas de texto na aba 1 
                                                       
                                                       fluidRow(
                                                         
                                                         tabBox(
                                                           title = strong("Indicadores Educacionais"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "200px",width = 12,
                                                           tabPanel("Visão geral", p(em("Os"),em(strong("indicadores educacionais"),em("atribuem valor estatístico à qualidade do ensino, atendo-se não somente ao desempenho dos alunos, mas também ao contexto econômico e social em que as escolas estão inseridas. Eles são úteis principalmente para o monitoramento dos sistemas educacionais, considerando o acesso, a permanência e a aprendizagem de todos os alunos. Dessa forma, contribuem para a criação de políticas públicas voltadas para a melhoria da qualidade da educação e dos serviços oferecidos à sociedade pela escola. (Referência: INEP) ")))
                                                                    ,p(em("O objetivo do estudo é obter informações sobre a educação básica dos municípios brasileiros, especificamente nos anos iniciais do ensino fundamental, com dados dos anos de 2014* e 2019 para os espaços geográficos: urbano, rural e total.")
                                                                       ,p(em("*Exclusivamente para o IDEB foi considerado o ano de 2013. "))))
                                                         ),
                                                         
                                                         tabBox(
                                                           title = strong("IDEB"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("O"),em(strong("Índice de Desenvolvimento da Educação Básica (Ideb)"),em("foi criado em 2007 e reúne os resultados de dois conceitos igualmente importantes para a qualidade da educação: o fluxo escolar e as médias de desempenho nas avaliações. O Ideb é calculado a partir dos dados sobre aprovação escolar, obtidos no Censo Escolar, e das médias de desempenho no Sistema de Avaliação da Educação Básica (Saeb). ")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/educacao_basica/portal_ideb/o_que_e_o_ideb/Nota_Tecnica_n1_concepcaoIDEB.pdf"),em("Os dados estão disponíveis no site do Inep, clice no ícone acima:",icon("graduation-cap")))))
                                                         ),
                                                         
                                                         tabBox(
                                                           title = strong("Migração para EJA"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("São"),em(strong(" os alunos que fazem a migração do ensino regular para a educação de jovens e adultos (EJA)."),em("EJA é a maneira ou metodologia de ensino correspondente às turmas destinadas a pessoas que não cursaram o ensino fundamental e/ou médio em idade própria. Aluno matriculado no ano t nas etapas seriadas de interesse, que no ano t + 1 se matricula na EJA.")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2007_2016/nota_tecnica_taxas_transicao_2007_2016.pdf"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         )),
                                                       
                                                       fluidRow(
                                                         tabBox(
                                                           title = strong("Docentes com curso superior"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("A"),em(strong("Proporção de docentes com curso superior completo que leciona no ensino fundamental anos iniciais - 1º ao 5º ano."),em("A formação de docentes para atuar na educação básica deve ser feita em nível superior, em curso de licenciatura, de graduação plena, em universidades e institutos superiores de educação. Sendo um requisito mínimo de formação para os professores da educação infantil.")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://portal.inep.gov.br/documents/186968/484154/Dicion%C3%A1rio+de+Indicadores+Educacionais+f%C3%B3rmulas+de+c%C3%A1lculo/bf7eac55-d33b-42a7-8d54-2d70fa4e24a3?version=1.2"),em("Os dados estao disponíveis no site, clique no ícone acima:",icon("user-graduate")))))
                                                         ),
                                                         
                                                         tabBox(
                                                           title = strong("Taxa de distorção idade-série"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("A"),em(strong("distorção idade-série é a proporção de alunos com mais de 2 anos de atraso escolar."),em("No Brasil, a criança deve ingressar no 1º ano do ensino fundamental aos 6 anos de idade, permanecendo no Ensino Fundamental até o 9º ano, com a expectativa de que conclua os estudos nesta modalidade até os 14 anos de idade. O cálculo da distorção idade-série é realizado a partir de dados coletados no Censo Escolar. Todas as informações de matrículas dos alunos são capturadas, inclusive a idade deles.")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://portal.inep.gov.br/documents/186968/484154/Dicion%C3%A1rio+de+Indicadores+Educacionais+f%C3%B3rmulas+de+c%C3%A1lculo/bf7eac55-d33b-42a7-8d54-2d70fa4e24a3?version=1.2"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         )),
                                                       
                                                       fluidRow(
                                                         tabBox(
                                                           title = strong("Taxa de Evasão"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("O indicador faz parte da Taxa de transição."),em(strong("Entende-se por evasão escolar a situação do aluno que abandou a escola ou reprovou em determinado ano letivo, e que no ano seguinte não efetuou a matrícula para dar continuidade aos estudos "),em("A taxa de evasão de aprovados na série k no ano t é a proporção de alunos matriculados (matrícula total) na série k no ano t que foram aprovados e não se matricularam no ano t+1. A taxa de evasão de não aprovados na série k no ano t é a proporção de alunos matriculados (matrícula total, ver definição 8) na série k no ano t que não foram aprovados e não se matricularam no ano t+1. ")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2007_2016/nota_tecnica_taxas_transicao_2007_2016.pdf"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         ),
                                                         
                                                         tabBox(
                                                           title = strong("Taxa de Promoção"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("O indicador faz parte da Taxa de transição."),em(strong("Denominado aluno promovido aquele que foi aprovado e se matriculou normalmente no próximo ano letivo em sua série correspondente."),em(" A taxa de promoção da série k no ano t é a proporção de alunos matriculados (matrícula total) na série k no ano t que vão se matricular na série k+1 no ano t+1.")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/informacoes_estatisticas/indicadores_educacionais/2007_2016/nota_tecnica_taxas_transicao_2007_2016.pdf"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         )),
                                                       
                                                       fluidRow(
                                                         tabBox(
                                                           title = strong("Taxa de Rendimento"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em(strong("Taxa de Rendimento (Abandono, Aprovação, Reprovação)"),em("As taxas de rendimento escolar são calculadas com base nas informações de rendimento e movimento dos alunos, coletadas na 2ª etapa do Censo Escolar, denominada módulo Situação do Aluno. Nesta etapa, os respondentes preenchem as informações de rendimento (aprovado/ reprovado), movimento (falecido, deixou de frequentar, transferido) e Curso em andamento/ Sem movimentação, de acordo com a etapa de ensino.")))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/educacao_basica/educacenso/situacao_aluno/documentos/2015/taxas_rendimento_escolar.pdf"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         ),
                                                         
                                                         tabBox(
                                                           title = strong("Taxa de Rendimento"),
                                                           # The id lets us use input$tabset1 on the server to find the current tab
                                                           id = "tabset1", height = "250px",
                                                           tabPanel("Descrição", p(em("O abandono ocorre quando o aluno deixa de frequentar as aulas durante o ano letivo, diferente da evasão. A"),em(strong("Taxa de abandono")), em("indica a porcentagem de alunos que deixaram de frequentar a escola após a data de referência do Censo. A"),em(strong("Taxa de aprovação")), em("indica a porcentagem de alunos que, ao final do ano letivo, alcançaram os critérios mínimos para a conclusão satisfatória da etapa de ensino na qual se encontrava. Aluno aprovado tem frequência e notas satisfatórias. A"),em(strong("Taxa de aprovação")), em("indica a porcentagem de alunos que, ao final do ano letivo, alcançaram os critérios mínimos para a conclusão satisfatória da etapa de ensino na qual se encontrava. Aluno aprovado tem frequência e notas satisfatórias. "))),
                                                           
                                                           tabPanel("Referências", p(em("Nota técnica disponivel em:"),em(tags$a("Clique aqui!",href="http://download.inep.gov.br/educacao_basica/educacenso/situacao_aluno/documentos/2015/taxas_rendimento_escolar.pdf"),em("Os dados estao disponíveis no site do Inep, clique no ícone acima:",icon("user-graduate")))))
                                                         )
                                                       )),
                                               
                                               
                                               
                                               tabItem(tabName = "aba_3", # Adicionando as caixas de texto na aba 3  
                                                       
                                                       fluidRow(
                                                         tabBox(
                                                           side = "right", height = "325px", width = '90%',
                                                           title = "Algumas informações complementares aos dados",
                                                           # The id lets us use input$tabset1 on the server to find the current ta
                                                           
                                                           tags$br(),
                                                           tags$div(tags$ul(
                                                             tags$li(a(span(icon("city"), style = "color:black")),strong(span("Nº de munícipios brasileiros: ", style = "color:black")),em("O Brasil possui atualmente 5570 municípios, o estado de MG é o com mais municípios 853, já o com menos munícipios é RR com 15. ")),
                                                             tags$br(),
                                                             tags$li(a(span(icon("dollar-sign"), style = "color:black")),strong(span("Média salarial dos professores - 2014: ", style = "color:black")),em("A média salarial nacional é de R$ 2771.00, a maior media salárial é de R$ 6286.68 no DF e a menor media do salário fica em PB com R$ 1560.68.")),
                                                             tags$br(),
                                                             tags$li(a(span(icon("chalkboard-teacher"), style = "color:black")),strong(span("Nº de docentes em exercício - 2014: ", style = "color:black")),em("O Brasil tem 749.837 professores em exercício, maior número de docentes é em SP com 173186 e o menor é roraima com 4418.")),
                                                             tags$br(),
                                                             tags$li(a(span(icon("school"), style = "color:black")),strong(span("Média de alunos por turma nos anos iniciais - 2014: ", style = "color:black")),em("A média de alunos por turma é de 21.5, a maior média é no AM com 24.6 e a menor é em PI com 19 alunos por turma. ")),
                                                             tags$br(),
                                                             tags$li(a(span(icon("school"), style = "color:black")),strong(span("Média de alunos por turma nos anos iniciais - 2019: ", style = "color:black")),em("A média de alunos por turma é de 21.6, a maior média de alunos por turma com 24.3 é no AM e a menor é na BA com 19.6 alunos por turma."))),  style = "font-size: 15px")
                                                         )
                                                       ),
                                                       
                                                       fluidRow(
                                                         box(title = "Gráfico de comparação entre os anos de 2014 e 2019", status = "primary", solidHeader = TRUE, plotlyOutput("grafico_aba3_1")), #Gráfico 1
                                                         box(title = "Gráfico de barras divergentes", status = "warning", solidHeader = TRUE,plotlyOutput("grafico_aba3_2")), #Gráfico 2
                                                         box(title = "Gráfico de comparação no ano de 2014", status = "primary", solidHeader = TRUE,plotlyOutput("grafico_aba3_3")), #Gráfico 3
                                                         box(title = "Gráfico de comparação no ano de 2019", status = "warning", solidHeader = TRUE,plotlyOutput("grafico_aba3_4")) #Gráfico 4
                                                         
                                                       )
                                               ),
                                               
                                               tabItem(tabName = "aba_5", # Adicionando o mapa na quinta aba
                                                       
                                                       fluidRow(
                                                         box(leafletOutput("mymap", width="100%", height = 550),
                                                             width = 12,
                                                             title = "Map",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             collapsible = FALSE,
                                                             height = "100%"
                                                         )) # Mapa
                                               )
                                             )
                                             ))



ui <- dashboardPage(header,sidebar, body)

