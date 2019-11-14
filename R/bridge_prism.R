#' @title Run ACCEPT Model
#' @description This function calls the predictACCEPT model. 
#' If no model_input is specified, it will use the defaults
#' If one of the columns in model_input is missing, it will replace with the default column
#' It will check all the columns of the patient_data object; if one is missing, it will
#' replace with the default, and will remove any extra columns
#' @param model_input A list/json object with "patient_data", "random_sampling_N", "random_distribution_iteration",
#' and "calculate_CIs" as columns
#' @return Returns a list of results
model_run<-function(model_input = NULL)
{

    input<-unflatten_list(model_input)
  
    results <- BODEIndex         (FEV1                   =model_input$FEV1,                  
                                  mMRC                   =model_input$mMRC,                  
                                  BMI                    =model_input$BMI,                  
                                  walk                   =model_input$walk)
    
    return(as.list(results))
}


get_default_input <- function() {
  model_input <- list(FEV1                   = 40, 
                      mMRC                   = 3, 
                      BMI                    = 22, 
                      walk                   = 100)
  return((flatten_list(model_input)))
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }
  
  for(i in 1:length(lst))
  {
    nm<-names(lst[i])
    
    message(nm)
    
    if(prefix!="")  nm<-paste(prefix,nm,sep=".")
    
    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()
  
  nms<-names(lst)
  
  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }
  
  return(out)
}
