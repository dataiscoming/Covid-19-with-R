# This function will create a title according the the variable selected with the function select_varINPUTui
# Input :  a variable selected with the function select_varINPUTui
# ouput : a title (character)

reactive_title <- function(input){
  
  # For each variable, there is a different title
  if(input == 'cc'){title = "total cases"
  }else if(input == 'ac'){title = "active cases"
  }else if(input == 'd'){title = "death"
  }else if(input == 'r'){title = "recovered"
  }else if(input == 'ncc'){title = "new confirmed cases"
  }else if(input == 'nac'){title = "new active cases"
  }else if(input == 'nd'){title = "new death"
  }else if(input == 'nr'){title = "new recovered"}
  
  # Result
  return(title)
}
