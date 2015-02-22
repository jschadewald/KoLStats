library(shiny)


shinyUI(fluidPage(
    titlePanel("Statistical Tools for KoL: A Data Scientist is You!"),
    br(),
    p("Welcome! This site serves a dual purpose as a course project
      for the \"Developing Data Products\" class -- offered by John's Hopkins",
      "University via", a(href="www.coursera.com","Coursera"), "--",
      "and as a tribute to a neat, free online game: ",
      a(href="http://www.kingdomofloathing.com/login.php", 
        "The Kingdom of Loathing.")),
    p("The goal of this site is to help you (the player) determine your chances", 
      "of obtaining a",
      a(href="http://kol.coldfront.net/thekolwiki/index.php/Misshapen_animal_skeleton",
        "misshapen animal skeleton"), "based on the items you already have.",
      "(Alternatively, you can just play with the widgets and watch the text",
      "and plots update. I do. It's fun!)"),
    img(src="http://images.kingdomofloathing.com/itemimages/animskel.gif",
        alt="A misshapen animal skeleton from Kingdom of Loathing"),
    br(),br(),
    p("To get started (and finished), use the lefthand panel to specify how",
      "many piles of dusty animal bones you plan to use and how many unique",
      "pieces of the skeleton you already have. As well, feel free to change",
      "the probability used for the predictions."),
    p("For now, results are presented in a two-stage process that first", 
      "determines the likely number of random bones that will be obtained from",
      "using your specified number of dusty piles (first graph). Then, it",
      "uses the expected number of \"random bones\" obtained to predict and", 
      "display the probability that you will obtain all 100 unique pieces", 
      "required to make a misshapen animal skeleton (second graph)."),
    p(tags$i("Note: Images used with permission.")),
    br(),
    sidebarLayout(
        sidebarPanel(
            p("Enter the number of",
              a(href="http://kol.coldfront.net/thekolwiki/index.php/Pile_of_dusty_animal_bones",
                "piles of dusty animal bones"),
              " in your inventory."), img(src="http://images.kingdomofloathing.com/itemimages/bonepile.gif"),
            numericInput(
                "piles", NULL,
                50, min=1, step=1), br(),
            p("How many of the 100 unique",
              a(href="http://kol.coldfront.net/thekolwiki/index.php/Dusty_animal_bones",
                "dusty animal bones"),
              "do you already have?"),
            img(src="http://images.kingdomofloathing.com/itemimages/aniskull.gif"),
            sliderInput(
                "bones", NULL,
                min=0, max=99, value=0, step=1),
            br(),
            p("What probability should be presented?"),
            sliderInput(
                "percentile", NULL,
                min=50, max=99, value=95, step=1, post="%")
        ),
        mainPanel(
            textOutput("preRbDensity"),
            plotOutput("plotRbExpected"),br(),
            textOutput("preUbCDF"),
            br(),
            plotOutput("plotGetRest"),
            br()
        )
    ),
    p("Copyright", HTML("&copy;"), "2015 Jason Schadewald"),
    p("This software is shared under the MIT License. The source code is here:",
    a("https://github.com/jschadewald/KoLStats"))
))