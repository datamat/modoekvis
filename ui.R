library(dygraphs)

shinyUI(fixedPage(
  tags$head(
    tags$style(type="text/css",".container { width: 750px; }")
  ),
  titlePanel("MODOEK data"),
  fixedRow(
    column(12,
    selectInput("select",
                label=h4("List of figures:"),
                choices=list(
                  "RWC: Pine 25 cm"=1,
                  "RWC: Oak 25 cm"=2,
                  "RWC: Pine 5 cm"=3,
                  "RWC: Pine 30 cm"=4,
                  "RWC: Oak 5 cm"=5,
                  "RWC: Oak 30 cm"=6,
                  "Soil Temperature: Pine 25 cm"=7,
                  "Soil Temperature: Oak 25 cm"=8,
                  "Soil Temperature: Pine 5 cm"=9,
                  "Soil Temperature: Pine 30 cm"=10,
                  "Soil Temperature: Oak 5 cm"=11,
                  "Soil Temperature: Oak 30 cm"=12),
                selected=1)
    ),
    column(12,
           htmlOutput("max"),
           dygraphOutput("fig",width=750),br()
    ),
    column(12,p(HTML("<div style='font-size: x-small;'><br><br>",
                     "Questions/comments, ",
                     "please write to ",
                     "<a href='mailto:matthias.haeni@wsl.ch' target='_blank'>",
                     "matthias.haeni@wsl.ch</a> and <a ",
                     "href='mailto:leonie.schoenbeck@wsl.ch' ",
                     "target='_blank'>leonie.schoenbeck@wsl.ch</a>.<br><br>",
                     "</div>"
    )))
  )
))
