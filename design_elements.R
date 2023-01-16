#########################################
## Design elements used in other files ##
#########################################

# Colors
blue = "#077187"
lightblue = "#0AA3C2"
orange = "#F49D37"
red = "#D72638"
darkblue = "#140F2D"
pink = "#EFA8B8"

# Witch with a speech bubble
witch_speaks_code = function(h3_text, h2_text, code_text){
  
  # Outer div defines grid 
  div(style = "position: relative;
            display: grid;
            grid-template-columns: 1fr 1fr 1fr;
            grid-template-rows: 1fr 1fr;
            ",
      
      # h3 text
      div(style = "grid-column-start: 1;
            grid-column-end: 1;
            grid-row-start: 1;
            grid-row-end: 2;
            z-index: 3;
            padding-top: 20px;
            padding-left: 0px;
            text-align: center;",
          h3(h3_text)
      ),
      
      #h2 text
      div(style = "grid-column-start: 1;
            grid-column-end: 1;
            grid-row-start: 1;
            grid-row-end: 2;
            z-index: 3;
            padding-top: 40px;
            padding-left: 0px;
            text-align: center;",
          br(),
          h4(h2_text)
      ),
      
      # code text
      div(style = "grid-column-start: 1;
            grid-column-end: 1;
            grid-row-start: 1;
            grid-row-end: 2;
            z-index: 3;
            padding-top: 60px;
            padding-left: 50px;
            padding-right: 50px",
          br(), 
          br(),
          code(code_text)
      ),
      
      # Speech bubble image
      div(style = "grid-column-start: 1;
          grid-column-end: 1;
          grid-row-start: 1;
          grid-row-end: 2;
          z-index: 2",
          img(src="speech_bubble.png", height = 300)
          
      ),
      
      # witch image
      div(style = "grid-column-start: 1;
          grid-column-end: 3;
          grid-row-start: 1;
          grid-row-end: 2;
          z-index: 1;
          padding-left: 395px;
          padding-top: 65px;",
          img(src="data_witch.png", height = 300)
          
      )
  )
}
