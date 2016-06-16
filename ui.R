library(dygraphs)

shinyUI(fixedPage(
  tags$head(
    tags$style(type="text/css", ".container { width: 750px; }")
  ),
  titlePanel("MODOEK data"),
  fixedRow(
    column(12,p(HTML("List of figures:<br>",
                     "<a href='#rwc'><b>Relative Water Contents:</b></a> ",
                     "<a href='#fig1'>Pine 25 cm</a>, ",
                     "<a href='#fig2'>Oak 25 cm</a>, ",
                     "<a href='#fig3'>Pine 5 cm</a>, ",
                     "<a href='#fig4'>Pine 30 cm</a>, ",
                     "<a href='#fig5'>Oak 5 cm</a>, ",
                     "<a href='#fig6'>Oak 30 cm</a><br>",
                     "<a href='#temps'><b>Soil Temperatures:</b></a> ",
                     "<a href='#fig7'>Pine 25 cm</a>, ",
                     "<a href='#fig8'>Oak 25 cm</a>, ",
                     "<a href='#fig9'>Pine 5 cm</a>, ",
                     "<a href='#fig10'>Pine 30 cm</a>, ",
                     "<a href='#fig11'>Oak 5 cm</a>, ",
                     "<a href='#fig12'>Oak 30 cm</a><br><br>",
                     "The following figures are zoomable in x and y, ",
                     "the data represented are unvalidated and raw. Have fun!"
    ))),
    column(12,
           tags$div(tags$h3("Relative Water Content [%] (RWC)"),id="rwc"),
           dygraphOutput("fig1",width=750),
           tags$hr(),br(),
           dygraphOutput("fig2",width=750),
           tags$hr(),br(),
           dygraphOutput("fig3",width=750),
           tags$hr(),br(),
           dygraphOutput("fig4",width=750),
           tags$hr(),br(),
           dygraphOutput("fig5",width=750),
           tags$hr(),br(),
           dygraphOutput("fig6",width=750),
           tags$hr(),br(),
           tags$div(tags$h3("Soil Temperature [°C]"),id="temps"),
           dygraphOutput("fig7",width=750),
           tags$hr(),br(),
           dygraphOutput("fig8",width=750),
           tags$hr(),br(),
           dygraphOutput("fig9",width=750),
           tags$hr(),br(),
           dygraphOutput("fig10",width=750),
           tags$hr(),br(),
           dygraphOutput("fig11",width=750),
           tags$hr(),br(),
           dygraphOutput("fig12",width=750)
    ),
    column(12,p(HTML("<br><br>Questions/comments, please write to ",
                     "<a href='mailto:matthias.haeni@wsl.ch' target='_blank'>",
                     "matthias.haeni@wsl.ch</a> and <a ",
                     "href='mailto:leonie.schoenbeck@wsl.ch' ",
                     "target='_blank'>leonie.schoenbeck@wsl.ch</a>.<br><br>"
    )))
  )
))
