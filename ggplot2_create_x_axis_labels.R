create_x_axis_staggered_labels = function(text_vector){
    
    # @param:
    # text_vector: a character vector with the name of strings
    
    for(i in 1:length(text_vector)){
        
        if(i %% 2 == 0){
            
            text_vector[i] = paste("\n", text_vector[i], sep = "")
            
        }
        
    }
    
    return(text_vector)
    
}