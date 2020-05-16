# This function will select the color for the varaible selected from the function select_varINPUTui
# Input :  an input from the function select_varINPUTui
# ouput : a color (character) 

reactive_color <- function(input){
    if(input %in% c("cc","ncc","ac","nac")){col = '#EF3B2C'
    }else if(input %in% c("d","nd")){col = "#737373"
    }else if(input %in% c("r","nr")){col= "#41AB5D"}
    return(col)
}


