library(shiny)
source("load_data.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Similarity-based interference in sentence comprehension"),
  # wellPanel(
  # flowLayout(
  HTML("<p>An R implementation of memory interference processes in linguistic dependency resolution based on ACT-R (Anderson et al., Psychological Review, 2004) and the sentence comprehension model by Lewis and Vasishth (Cognitive Science, 2005).
    It features the extensions <b>Associative Cues</b> and <b>Distractor Prominence</b> as described in F. Engelmann (Doctoral Thesis, 2016), 
    <a href=\"http://journal.frontiersin.org/article/10.3389/fpsyg.2015.00617/abstract\">L. J&auml;ger, F. Engelmann, and S. Vasishth (Frontiers in Psychology, 2015)</a>, 
    and F. Engelmann, L. J&auml;ger, and S. Vasishth: <i>Similarity-based interference in sentence comprehension:  A new computational model</i> (manuscript).
    Experimental studies reported here are documented in L. J&auml;ger, F. Engelmann, and S. Vasishth:
    <i>Similarity-based interference in sentence comprehension: A literature review and a Bayesian meta-analysis</i> (submitted). </p>"
    ),

    # An extended cue-based retrieval model of interference in dependency resolution such as in reflexive-antecedent or subject-verb dependencies.
    # <a href=\"http://www.ling.uni-potsdam.de/~engelmann/publications/EngelmannJaegerVasishth-CUNY2015.pdf\">Engelmann, J&auml;ger, Vasishth (2015)</a>.
  p(tags$b("Description:"),
    "The model simulates the retrieval process assuming two retrieval cues (structural, like c-command or subject, and non-structural, like number or gender) with two items in memory (target and distractor). Four conditions are simulated in a 2x2 design that manipulates the match of the second cue with target and distractor (target-match/mismatch x distractor-match/mismatch).
    The target is defined as the item that always matches the first cue (structural) and mismatches the second cue in the Target-Mismatch conditions. The distractor does not match the first cue (structural) in any condition and mismatches the second cue in the Distractor-Mismatch conditions."),
    p(" 
    The interference effect is measured within Target-Match and Target-Mismatch conditions as the difference between Distractor-Match (high interference) and Distractor-Mismatch conditions (low interference). 
    A positive effect in retrieval latencies represents as slow-down (inhibition), a negative effect represents a speed-up (facilitation).
    ")
  # HTML("The model and empirical data used for simulation are described in Engelmann, J&auml;ger, Vasishth, \"The Determinants of Retrieval Interference in Dependency Resolution: Review and Computational Modeling\" (submitted). The model is also featured in <a href=\"http://journal.frontiersin.org/article/10.3389/fpsyg.2015.00617/abstract\">J&auml;ger, Engelmann, Vasishth (2015)</a> and <a href=\"http://www.ling.uni-potsdam.de/~engelmann/publications/EngelmannJaegerVasishth-CUNY2015.pdf\">Engelmann, J&auml;ger, Vasishth (2015)</a>.")
  # )
  ,

  tabsetPanel(
    tabPanel('Model',

      sidebarLayout(
        sidebarPanel(width=3,

          # wellPanel(
          actionButton("reset", label = "Reset"),
          actionButton("runmodel", label = "Run"),
              # checkboxInput('default', 'Display default model', TRUE),
          sliderInput("iterations",
            "Run iterations", min = 500, max = 10000, value = 5000, step=500),
          selectInput("data", "Show dataset", choices = expnames, selected = "---"),
          selectInput("setparams", "Preset parameters", choices = expnames, selected = "---"),
          tags$b("Preset interference type"),
          actionButton("intRet", label = "Retroactive"),
          actionButton("intPro", label = "Proactive"),
          checkboxInput('useDecay', 'Activate decay', TRUE),

          hr(),

          # wellPanel(
            # h4("ACT-R parameters"),
          # radioButtons("inttype",
                # "Linear order", choices = c("Retroactive", "Proactive", "None"), selected = "Retroactive"),
          sliderInput("lf",
            "Latency factor",
            min = 0.1,
            max = 1.0,
            value = 0.15, step=0.05, animate=TRUE),
          sliderInput("rth",
            "Retrieval threshold",
            min = -5.0,
            max = 1.0,
            value = -1.5, step=0.25, animate=TRUE),
          sliderInput("ans",
            "Activation noise",
            min = 0,
            max = 1.0,
            value = 0.15, step=0.05, animate=TRUE),
          sliderInput("mas",
            "Maximum associative strength",
            min = 0,
            max = 3,
            value = 1, step=0.5),
          sliderInput("mp",
            "Mismatch penalty",
            min = 0,
            max = 3,
            value = 1, step=0.25, animate=TRUE),
          sliderInput("ga",
            "Goal source activation W",
            min = 0,
            max = 2,
            value = 1, step=0.25, animate=TRUE)
            # )
          ),

        mainPanel(
          fluidRow(

            column(width=6,
                plotOutput("plot1", width=350, height=300),
                plotOutput("plot2", width=350, height=300),
                plotOutput("plot3", width=350, height=300)
                )
            ,

            column(width=3,
              radioButtons("model", "Switch model", 
                choices=c("Classic LV05", "Extended"),
                selected="Extended"),
              tags$b("Study Properties"),
              verbatimTextOutput("studyProperties"),
              sliderInput("dprom", "Distractor prominence", 
                min = -4, max = 4, value = 0, step=0.25, 
                animate=TRUE),
              sliderInput("cl",
                "Cue confusion level",
                min = 0, max = 100, value = 0, step=5,
                post="%", animate=TRUE),
              sliderInput("cueweighting", "Structural cue weight",
                min = 0, max = 5, value = 1, step=.5, 
                animate=TRUE)
              ,
              hr(),
              # h4("Memory items"),
              sliderInput("ndistr", "Number of distractors", min = 1, max = 4, value = 1, step=1),
              sliderInput("ldp", "Distractor decay (sec)", min = 0.1, max = 3, value = 1, step=0.1, animate=TRUE),
              sliderInput("lp", "Target decay (sec)", min = 0.1, max = 3, value = 1, step=0.1, animate=TRUE)
            # hr(),
            # sliderInput("qcf",
                # "Match quality correction",
                # min = 0, max = 10, value = 0)
              ),
            column(width=3,
              # p("The <b>match quality correction</b> factor defines how strongly an item's general relevance (activation) influences its match with the retrieval cues. <br/>")
              p(HTML("<b>Distractor prominence</b> is the general (cue-independent) relevance of the distractor. A high distractor prominence can lead to a facilitation in Target-Match conditions.")),
              p(HTML("<b>The cue confusion level</b> defines to what degree the two retrieval cues are <i>cross-associated</i> with each other's corresponding features (e.g., the +c-com cue is associated with the feature +female). Cross-associated (confused) cues lead to an inhibition in Target-Mismatch conditions.")),
              p(HTML("<b>The structural cue weight</b> is the relative weight of the first cue over the second (ratio of structural/semantic).")),
              p(HTML("Change <b>distractor and target decay</b> accordingly for pro- and retroactive interference simulations or use the preset buttons on the left (decay must be activated)."))
              )

            )
          )
        )
      )
    ,
    tabPanel('Literature summary',
     dataTableOutput("littable"))
    # tabPanel('References',
     # NULL)
    )
  )
)
