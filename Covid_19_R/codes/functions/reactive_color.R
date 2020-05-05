# Reactive colors definition

reactive_color <- function(input){
    if(input %in% c("cc","ncc","ac","nac")){col = '#EF3B2C'
    }else if(input %in% c("d","nd")){col = "#737373"
    }else if(input %in% c("r","nr")){col= "#41AB5D"}
    return(col)
}


