rriskDistributionsClass$set("public", 
  "set_info", function(info)
  {
    private$info_list <- info
  }
)

rriskDistributionsClass$set("public", 
  "get_info", function(info)
  {
    private$info_list
  }
)