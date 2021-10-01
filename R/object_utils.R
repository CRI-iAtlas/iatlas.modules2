validate_object_type <- function(obj, obj_name){
  if(typeof(obj) != "environment"){
    stop("Object must be of type environment")
  }
  if(!all(class(obj) == c(obj_name, "R6"))){
    stop("Object must be of class ", obj_name, " not ", class(obj))
  }
}
