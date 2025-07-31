build_action_buttons <- function(id, is_implicit_node = FALSE, item = "") 
{
  get_action_button <- function(btn_name, label) 
  {
    actionButton(inputId = paste(btn_name, id, sep = "_"),
                 label   = label,
                 onclick = paste0("Shiny.setInputValue('", btn_name, 
                                  "', this.id, {priority: 'event'})"))
  }
  
  tags <- tagList()
  
  # add change button
  #tags <- tagAppendChild(tags,
  #                       get_action_button(btn_name = paste0("btn_change_", 
  #                                                           item), 
  #                                         label    = "CHANGE"))
  
  # add delete button for non-implicit nodes
  if (!is_implicit_node) {
    tags <- tagAppendChild(tags, 
                           get_action_button(btn_name = paste0("btn_delete_", 
                                                               item), 
                                             label    = "DELETE"))
  }
  
  as.character(tags)
}