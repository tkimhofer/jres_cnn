# Define UI for application that draws a histogram
ui<-fluidPage(
  title='jRes',
  #titlePanel('Jres Feature Check'), # h1(textOutput('IDspec'))
  titlePanel(h3(textOutput(outputId='IDspec'))),
  # for now use already read-in spectrum 
  #fileInput(fileID, label='Select J-res file', multiple = FALSE, accept = NULL, width = NULL),
  br(),
  #fluidRow(
  column(2,
         #fluidRow(textOutput(outputId='IDspec')),
         DTOutput(outputId = 'summarytbl'),
         hr(),
         plotOutput('featbb_Int'),
         fluidRow(column(offset=1,width = 12,
                         numericInput('manID', min=0, max=nrow(mult), value = NA, label=NULL)
                         )
                  ),
         br(),
         fluidRow(column(offset=0,width = 12, align='center',
         h3('Evaluation Results'),
         DTOutput("findings"),
         br(),
         actionButton(inputId='submit', label='Submit'))),
         br(),
         hr(),
         br(),
         actionButton(inputId='prevFeat', label='Previous'),
         actionButton(inputId='nextFeat', label='Next')
  ),
  column(10,
         tabsetPanel(id='ppopt',
                     tabPanel("Feature Table", 
                              br(),
                              DTOutput('featTbl')
                     ),
                     tabPanel("Feature Description", 
                              br(),
                              fluidRow(column(width=12, align='center', h1(textOutput('FeatureID')))), 
                              br(),
                              br(),
                              fluidRow(
                                add_busy_bar(color = "#FF0000"),
                                column(width=6, plotOutput('featbb_overview')),
                                column(width=6, plotOutput('featbb_overviewLOG'))
                              ),
                              hr(),
                              br(),
                              fluidRow(column(width=12, align='center', h2('Peak position and bounding box'))),
                              br(),
                              fluidRow(

                                column(width=3, align='center',
                                       DTOutput(outputId = 'Fsummarytbl'),  
                                       fluidRow(textOutput('info'))
                                       ),
                                
                                column(width=3, align='left',
                                       h3('Does the bounding box fit the peak?'),
                                       br(),
                                       radioButtons('bboxfit', label=NULL, choices=c(
                                         'Optimal (fully enclosing a single peak)', 
                                         'Slightly too small in f1',
                                         'Slightly too small in f2',
                                         'Way too small',
                                         'Too large', 
                                         'Hard to see'), inline=F)
                                ),
                                
                                column(width=5, align='center',
                                       plotOutput('log_sums')
                                       )
                              ),
                              hr(),
                              br(),
                              fluidRow(column(width=12, align='center', h2('Peak shape and multiplicity'))),
                              br(),
                              fluidRow(
                                
                                column(width=5, align='center',
                                       column(width=12, plotlyOutput(outputId = 'featbb'))
                                ),
                                
                                column(width=2, align='left',
                                       h3('Is this a well-defined peak?'),
                                       br(),
                                       radioButtons('signal_def', label=NULL, choices=c(
                                         'Yes - such a nice peak!', 
                                         'Yes, but it is very small (most likely satellite)', 
                                         'One side of the peak overlaps with another feature',
                                         'Can\'t say, severe peak convolution/overlap',
                                         'No, that is def not a peak', 
                                         'Hard to say for other reasons'), inline=F)
                                ),
                                
                                
                                
                                column(width=5, align='center',
                                       column(width=12, plotlyOutput(outputId = 'featbb_minmax'))
                                )
                              ),
                              hr(),
                              br(),
                              fluidRow(
                                column(width=12, align='center', h2('Signal multiplicity')),
                                column(width=4, align='left',
                                       h3('Multiplicity and compound indentification'),
                                       br(),
                                       radioButtons('mult_fine', label=NULL, choices=c(
                                         'Fine as is', 
                                         'Unmatched - prob singlet',
                                         'Unmatched - additional peak that might not belong there'
                                         ), inline=F)
                                       
                                ),
                                column(width=6, align='center', 
                                       fluidRow(DTOutput('info_mult')),
                                       hr(),
                                       fluidRow(
                                         h3('Database matches'), 
                                         fluidRow(sliderInput('matchdev', 'Max shift deviation in f2 (Hz)', min=0, max=20, value=3),
                                                  textOutput('ppm'),
                                                  br(),
                                                  DTOutput('dbmatch')
                                                  )
                                         ),
                                       br(),
                                       br()
                                )
                              
                             
                              
                     )),
                     tabPanel("Database list", 
                              br(),
                              DTOutput('db_full')
                     ),
                     
                     tabPanel("Peak Classif",
                              br(),
                              DTOutput('featimg'),
                              actionButton('save_img_train', 'Save images')
                     )
                     
                     #   tabPanel("Viz", 
                     #            
                     # )
                     
         )
  )
  
  
  
)

