createUI <-
function(){
    
    shiny.titel<-dashboardHeader(title = 'MtreeRing')
    
    
    
    shiny.sider<-dashboardSidebar(
        sidebarMenu(
            
            menuItem('Image Loading',tabName = 'input_pre', 
                     icon = icon('folder-open', lib = 'font-awesome')),
            
            menuItem('Measurement',tabName = 'mea_arg', 
                     icon = icon('gear', lib = 'font-awesome'))
        )
    )
    
    
    
    
    page1<-fluidRow(
        
        box(title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Image Preview'),
            width = 12, 
            status = 'primary', solidHeader = T, collapsible = T,
            plotOutput('pre.img',
                       brush = brushOpts(
                           id = "plot1_brush",
                           opacity = 0.25,
                           resetOnNew = TRUE)
            )  
        ), 
        box(
            
            title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Image Upload'),
            width = 4, 
            status = 'primary', solidHeader = T, collapsible = T,
            
            conditionalPanel(
                condition = '!input.inmethod',
                fileInput('select.file', 'Choose an image file',
                          buttonLabel = 'Browse...',width = '60%')
            ),
            prettySwitch(inputId = "magick.switch",label = "Magick ON",
                         value = TRUE, fill = TRUE, status = "success"),
            helpText('Image upload is limited to 150 MB per file. Supported',
                     ' formats include png, jpeg, jpg, tif and bmp.',
                     style = 'color:#000000'),
            
            awesomeCheckbox(inputId = "inmethod", 
                            label = div(style = 'color:#000000;font-weight: bolder;', 'Image Path'), 
                            value = F, status = "success"),
            conditionalPanel(
                condition = 'input.inmethod',
                textInput('enter.path','Enter file path',''),
                helpText('For example: C:/Users/shiny/img01.png',
                         style = 'color:#000000'),
                hr()
            ),
            actionButton('buttoninputimage','Load ',
                         class = "btn btn-primary btn-md",
                         icon = icon('upload',"fa-1x"),
                         style = 'color:#FFFFFF;text-align:center;font-weight: bolder;font-size:110%;'),
            useSweetAlert()
            
        ),
        
        
        box(title = div(style = 'color:#FFFFFF;font-size:105%;font-weight: bolder', 'Image Rotation'),
            width = 3, status = 'primary', solidHeader = T, collapsible = T,
            
            prettyRadioButtons(inputId = "rotatede", label = "",
                               choices = c("0 degrees" = "rotate0",
                                           "90 degrees" = "rotate90",
                                           "180 degrees" = "rotate180",
                                           "270 degrees" = "rotate270"),
                               shape = "round", status = "success",
                               fill = TRUE, inline = TRUE),
            helpText("Rotation angle in degrees. Note that the bark side",
                     " needs to be located towards the left side of the ",
                     "window and the pith side towards the right.",
                     style = 'color:#000000'),
            actionButton('buttonrotate','Rotate  ',
                         class = "btn btn-primary btn-md",
                         icon = icon('repeat',"fa-1x"),
                         style = 'color:#FFFFFF;text-align:center;font-weight: bolder;font-size:110%;')
        ), 
        
        box(title = div(style='color:#FFFFFF;font-size:105%;font-weight: bolder','Image Cropping'),
            width = 3, status = 'primary', solidHeader = T, collapsible = T,
            
            helpText("This operation allows users to remove unwanted cores or irrelevant objects.",
                     "It may accelerate the subsequent image processing and plotting.",
                     style = 'color:#000000'),
            
            prettyRadioButtons(inputId = "cropcondition", label = "",
                               choiceNames = 'Current State: uncropped',
                               choiceValues = list('a'),
                               shape = "round", status = "danger",
                               fill = TRUE, inline = TRUE),
            
            
            awesomeCheckbox(inputId = "showcropp", 
                            label = div(
                                style='color:#000000;font-weight: bolder;',
                                'Show Help'), 
                            value = F, status = "success"),
            
            conditionalPanel(
                condition = 'input.showcropp',
                helpText("The \"brush\" operation allows users to create a transparent rectangle", 
                         "on the image and drag it around. For cores scanned side by side, the user", 
                         "could \"brush\" the preview image to choose an area of interest which", 
                         "contains only one core.",
                         style = 'color:#000000'),
                helpText("After brushing, click the button \"Crop\" to create a cropped area.",
                         "Subsequent steps of the measurement will be performed in this area,", 
                         "rather than the whole (uncropped) image.",
                         style = 'color:#000000'),
                helpText("To cancel this operation, users can click the button \"Cancel",
                         "\". If the transparent rectangle exists, user should first",
                         "click on the outer region of the rectangle (this will make the ",
                         "rectangle disappear) and then click the button \"Cancel\".",
                         style = 'color:#FF0000')
            ),  
            #textOutput("cropcondition"),
            hr(),
            actionButton('buttoncrop','Crop',
                         class = "btn btn-primary btn-md",
                         icon = icon('crop',"fa-1x"),
                         style = 'color:#FFFFFF;text-align:center;font-weight: bolder;font-size:110%;')
        )
	)
    
    
    page2.1<-fluidRow(
        
        box(
            title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Options'),
            width = 4, 
            status = 'primary', solidHeader = T, collapsible = T,
            
            textInput('tuid','Series ID', '', width = '75%'),
            
            textInput('dpi', 'DPI of the image', '','75%'),
            
            textInput('sample.yr', 'Sampling year', strtrim(as.character(Sys.Date()),4),'75%'),
            
            textInput('m.line', 'Y-coordinate of path', '','75%'),
            
            awesomeCheckbox(inputId = "incline", 
                            label = div(
                                style='color:#000000;font-weight: bolder;',
                                'Inclined tree rings'), 
                            value = F, status = "success"),
            
            conditionalPanel(
                condition = 'input.incline',
                numericInput('h.dis', 'Distance between paths (mm)', 
                             1,0.2,30,0.1,width='75%')
            ),
            
            
            
            br(),
            radioGroupButtons(inputId = "measuremethod", 
                              label = 'Measurement mode',
                              status = "btn btn-primary btn-md",
                              #individual = T,
                              size = 'normal',
                              choiceNames = list(
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      'Manual'), 
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      'Automation')),
                              choiceValues = list('manual', 'auto'),
                              width = '100%')
            
        ),
        box(
            title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Options'),
            width = 4, 
            status = 'primary', solidHeader = T, collapsible = T,
            
            sliderInput('linelwd','Line width for path',
                        0.2,3,1,0.1,width='80%'),
            sliderInput('marker.cex','Magnification for labels',
                        0.2,3,1,0.1,width='80%'),
            
            
            radioGroupButtons(inputId = "pch", 
                              label = 'Symbol for borders',
                              status = "btn btn-primary btn-md",
                              #individual = T,
                              size = 'sm',
                              choiceNames = list(
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      icon('circle', 'fa-lg')), 
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      icon('circle', 'fa-1x')), 
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      icon('circle-o', 'fa-1x')), 
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      icon('times', 'fa-1x')),
                                  div(
                                      style='color:#FFFFFF;font-weight: bolder;',
                                      icon('plus', 'fa-1x'))
                              ),
                              selected = '20',
                              choiceValues = list('19', '20', '1', '4', '3'),
                              width = '100%'),
            
            
            colorSelectorInput(inputId = "border.color", label = "Color for borders",
                               choices = c("black", "gray", "white", "red", "#FF6000", 
                                           "#FFBF00", "#DFFF00", "#80FF00", "#20FF00", 
                                           "#00FF40", "#00FF9F", "cyan", "#009FFF", "#0040FF",
                                           "#2000FF", "#8000FF", "#DF00FF", "#FF00BF"),
                               selected = 'black', mode = "radio", display_label = FALSE, ncol = 9),
            colorSelectorInput(inputId = "label.color", label = "Color for labels",
                               choices = c("black", "gray", "white", "red", "#FF6000", 
                                           "#FFBF00", "#DFFF00", "#80FF00", "#20FF00", 
                                           "#00FF40", "#00FF9F", "cyan", "#009FFF", "#0040FF",
                                           "#2000FF", "#8000FF", "#DF00FF", "#FF00BF"),
                               selected = 'black', mode = "radio", display_label = FALSE, ncol = 9)
        ),
        
        
        conditionalPanel(
            condition = 'input.measuremethod=="auto"',
            box(title = div(style='color:#FFFFFF;font-size:105%;
                            font-weight: bolder',
                            'Options'),
                width = 4,
                status = 'primary', solidHeader = T, collapsible = T,
                awesomeCheckbox(inputId = "isrgb", 
                                label = div(
                                    style='color:#000000;font-weight: bolder;',
                                    "Default RGB"), 
                                value = TRUE, status = "info"),
                conditionalPanel(
                    condition = '!input.isrgb',
                    textInput('customRGB','Custom RGB','0.299,0.587,0.114'),
                    helpText('Note:The three numbers correspond to',
                             'R, G and B components,respectively.',
                             style = 'color:#000000;font-weight: bolder'),
                    hr()
                ),
                
                
                radioGroupButtons(inputId = "method", 
                                  label = 'Method of automatic detection',
                                  status = "btn btn-primary btn-md",
                                  #individual = T,
                                  size = 'normal',
                                  choiceNames = list(
                                      div(
                                          style='color:#FFFFFF;font-weight: bolder;',
                                          'Watershed'), 
                                      div(
                                          style='color:#FFFFFF;font-weight: bolder;',
                                          'Canny'), 
                                      div(
                                          style='color:#FFFFFF;font-weight: bolder;',
                                          'measuRing')),
                                  choiceValues = list('watershed','canny','lineardetect'),
                                  width = '100%'),
                
                conditionalPanel(
                    condition = 'input.method=="watershed"',
                    selectInput('watershed.threshold',
                                'Otsu threshold value',
                                c('Auto (Recommended)' = 'auto',
                                  'Custom' = 'custom.waterthr'),
                                width = '75%'
                    ),
                    conditionalPanel(
                        condition = 'input["watershed.threshold"]=="auto"',
                        sliderInput('watershed.adjust',
                                    'Adjust the automatic threshold',
                                    0.5,1.5,0.8,0.05,width='75%')
                    ),
                    conditionalPanel(
                        condition = 'input["watershed.threshold"]=="custom.waterthr"',
                        textInput('watershed.threshold2',
                                  'Threshold value',''),
                        'A string argument of the form XX% (e.g., 98%)'
                    )
                    
                    
                ),
                
                
                conditionalPanel(
                    condition = 'input.method=="canny"',
                    awesomeCheckbox(inputId = "defaultcanny", 
                                    label = div(
                                        style='color:#000000;font-weight: bolder;',
                                        "Automatic canny threshold (Recommanded)"),
                                    value = TRUE, status = "info"),
                    
                    conditionalPanel(
                        condition = 'input.defaultcanny',
                        sliderInput('canny.adjust',
                                    'Adjust the automatic threshold',
                                    0.8,1.8,1.4,0.05,width='75%')
                    ),
                    
                    conditionalPanel(
                        condition = '!input.defaultcanny',
                        textInput('canny.t2',
                                  'Threshold for strong edges','','75%'),
                        textInput('canny.t1',
                                  'Threshold for weak edges','','75%')
                    ),
                    
                    numericInput('canny.smoothing',
                                 'Degree of smoothing',
                                 2,1.5,4,0.1,width='67%')
                ),
                
                conditionalPanel(
                    condition = 'input.method!="lineardetect"',
                    awesomeCheckbox(inputId = "defaultse", 
                                    label = div(
                                        style='color:#000000;font-weight: bolder;',
                                        "Default structuring elements"),
                                    value = TRUE, status = "info"),
                    
                    conditionalPanel(
                        condition = '!input.defaultse',
                        
                        textInput('struc.ele1',
                                  'First structuring element','','75%'),
                        textInput('struc.ele2',
                                  'Second structuring element','','75%')
                    ),
                    hr()
                    
                ),
                
                conditionalPanel(
                    condition = 'input.method=="lineardetect"',
                    textInput('origin',' Origin in smoothed gray','0','75%'),
                    'Linear detection don\'t support to check the box "Inclined tree rings".',
                    'Please verify this option before running the automatic detection.',
                    hr()
                ),
                
                'Automatic detection may take a few seconds or more  
                depending on the image size and complexity of the sample'
            )
            ),
        
        
        box(title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Measurement Window'),
            width = 12, 
            status = 'primary', solidHeader = T, collapsible = T,
            
            
            conditionalPanel(
                condition = 'input.measuremethod!="auto"',
                actionButton('buttoncreatpath','Create Path',
                             class = "btn btn-primary btn-md",
                             icon = icon('plus'),
                             style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                ),
                useSweetAlert(),
                
                actionButton('buttonrcm','Remove Result',
                             class = "btn btn-danger btn-md",
                             icon = icon('trash'),
                             style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                ),
                useSweetAlert()
                
            ),
            conditionalPanel(
                condition = 'input.measuremethod=="auto"',
                actionButton('buttoncreatpath2','Create Path',
                             class = "btn btn-primary btn-md",
                             icon = icon('plus'),
                             style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                ),
                useSweetAlert(),
                
                actionButton('buttonrcm2','Remove Result',
                             class = "btn btn-danger btn-md",
                             icon = icon('trash'),
                             style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                ),
                useSweetAlert(),
                
                
                actionButton('button_run_auto','Run Automatic Detection',
                             class = "btn btn-success btn-md",
                             icon = icon('play'),
                             style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                ),
                useSweetAlert()
            ),
            hr(),
            plotOutput('pre.img2',
                       dblclick = "plot2_dblclick",
                       brush = brushOpts(
                           id = "plot2_brush",
                           resetOnNew = TRUE
                       )
            ),
            hr(),
            actionButton('buttonsubimg','Create Sub-image',
                         class = "btn btn-primary btn-md",
                         icon = icon('search-plus'),
                         style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
            )
        ),
        
        
        
        
        box(
            title = div(style='color:#FFFFFF;font-size:105%;
                        font-weight: bolder',
                        'Zoomed Image Window'),
            width = 12, 
            status = 'primary', solidHeader = T, collapsible = T,
            actionButton('buttonzoomdel','Delete Border',
                         class = "btn btn-danger btn-md",
                         icon = icon('eraser'),
                         style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
            ),
            useSweetAlert(),
            hr(),
            
            plotOutput('zoom.img',
                       dblclick = dblclickOpts(
                           id = "zoom_dblclick"
                       ),
                       brush = brushOpts(
                           id = "zoom_brush",
                           resetOnNew = TRUE
                       )
            )
        )
	)
    
    page2.2<-fluidRow(
        column(width = 7,
               
               box(
                   title = div(style='color:#FFFFFF;font-size:105%;font-weight: bolder',
                               'Delete Borders'),
                   width = 4, 
                   status = 'primary', solidHeader = T, collapsible = T,
                   
                   
                   conditionalPanel(
                       condition = 'input.incline',
                       textInput('del.u',
                                 'Border number in the upper portion','','75%'),
                       textInput('del.l',
                                 'Border number in the lower portion','','75%')
                   ),
                   
                   conditionalPanel(
                       condition = '!input.incline',
                       textInput('del',
                                 'Border number','','75%')
                   ),
                   'To perform a mass deletion of borders, use commas to separate border numbers, e.g. 1,2,3,4',
                   br(),
                   br(),
                   actionButton('button_del','Delete Border',
                                class = "btn btn-danger btn-md",
                                icon = icon('eraser'),
                                style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                   )
                   
               ),
               
               conditionalPanel(  
                   condition = 'input.tuheader',
                   box(
                       title = 'Header',width = 4, 
                       status = 'primary', solidHeader = T, collapsible = T,
                       textInput('tuhdr1','Site ID',''),
                       textInput('tuhdr2','Site Name',''),
                       textInput('tuhdr3','Species Code',''),
                       textInput('tuhdr4','State or Country',''),
                       textInput('tuhdr5','Species',''),
                       textInput('tuhdr6','Elevation','')
                   )
               ),   
               
               conditionalPanel(  
                   condition = 'input.tuheader',
                   box(
                       title = 'Header',width = 4, 
                       status = 'primary', solidHeader = T, collapsible = T,
                       textInput('tuhdr7','Latitude',''),
                       textInput('tuhdr8','Longitude',''),
                       textInput('tuhdr9','First Year',''),
                       textInput('tuhdr10','Last Year',''),
                       textInput('tuhdr11','Lead Investigator ',''),
                       textInput('tuhdr12','Completion Date ','')
                   )
               )   
        ),
        
        column(width = 5,
               tabBox(
                   #title = tagList(shiny::icon("gear"), 'Output'),
                   title = div(
                       style='color:#000000;font-weight: bolder;',
                       icon('cog', class = 'fa-spin', lib = 'font-awesome'),
                       'Output'),
                   width = 12,
                   tabPanel(div(
                       style='color:#000000;font-weight: bolder;',
                       icon('list-ol','fa-1x'),
                       ' Results'),
                       
                       
                       actionButton('button_results','Generate Series',
                                    class = "btn btn-primary btn-md",
                                    style = 'color:#FFFFFF;text-align:center;
                                    font-weight:bolder;'
                       ),
                       useSweetAlert(),
                       
                       actionButton('button_hide','Hide Series',
                                    class = "btn btn-primary btn-md",
                                    style = 'color:#FFFFFF;text-align:center;
                                    font-weight:bolder;'
                       ),
                       useSweetAlert(),
                       
                       
                       
                       
                       br(),
                       tableOutput('results')),
                   tabPanel(div(
                       style='color:#000000;font-weight: bolder;',
                       icon('arrow-down', 'fa-1x'),
                       ' CSV'),
                       textInput('csv.name','Name of the csv file','', width = '50%'),
                       helpText(style='color:#000000;font-weight: normal;',
                                'The filename extension is not required. ',
                                'Leave blank to use current series ID.'),
                       helpText(style='color:#FF0000;font-weight: normal;',
                                'Attention: if running the app within an RStudio window',
                                ', the rename operation doesn\'t work. Please run the app',
                                ' within a browser.'),
                       hr(),
                       #HTML("<p style='color:#000000;'><b>CSV</b></p>"),
                       downloadButton('RingWidth.csv', 'Download CSV',
                                      class = "btn btn-primary btn-md",
                                      style = 'color:#FFFFFF;text-align:center;
                                      font-weight:bolder;')
                       
                       ),
                   tabPanel(div(
                       style='color:#000000;font-weight: bolder;',
                       icon('arrow-down','fa-1x'),
                       ' RWL'
                   ),
                   textInput('rwl.name','Name of the rwl file','', width = '50%'),
                   helpText(style='color:#000000;font-weight: normal;',
                            'The filename extension is not required. ',
                            ' Leave blank to use current series ID.'),
                   helpText(style='color:#FF0000;font-weight: normal;',
                            'Attention: if running the app within an RStudio window',
                            ', the rename operation doesn\'t work. Please run the app',
                            ' within a browser.'),
                   hr(),
                   
                   selectInput('tuprec','Precision of the rwl file',
                               c('0.01' = '0.01',
                                 '0.001'  = '0.001'),
                               selected = '0.01', width = '50%'),
                   helpText(style='color:#000000;font-weight: normal;',
                            'Units are in mm.'),
                   hr(),
                   
                   checkboxInput('tuheader', 'Header of the File', F),
                   conditionalPanel(  
                       condition = 'input.tuheader',
                       actionButton('reset.hdr','Reset Header',
                                    class = "btn btn-danger btn-md",
                                    icon = icon('trash'),
                                    style = 'color:#FFFFFF;text-align:center;font-weight: bolder'
                       )
                   ),
                   helpText(style='color:#000000;font-weight: normal;',
                            'For more details about the header, please', 
                            'read reference manual of the R package dplR.', 
                            'The output file is Tucson format.'),
                   hr(),
                   #HTML("<p style='color:#000000;'><b>RWL</b></p>"),
                   downloadButton('RingWidth.rwl', 'Download RWL',
                                  class = "btn btn-primary btn-md",
                                  style = 'color:#FFFFFF;text-align:center;
                                  font-weight:bolder;')
                   
                   )
                   
                   )
               
                   )
        )
    
    
    shiny.body<-dashboardBody(
        
        tabItems(
            
            tabItem(tabName = 'input_pre',
                    page1
            ),
            
            tabItem(tabName = 'mea_arg',
                    page2.1,
                    page2.2
            )
        )
    )
    
    
    
    
    ui <-dashboardPage(
        shiny.titel,
        shiny.sider,
        shiny.body
    )
    return(ui)
}
