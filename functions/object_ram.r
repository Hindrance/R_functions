# Simple function call to see and sort objects by the RAM usages


object_ram = function() {
              sapply(ls(envir = .GlobalEnv), function(x) format(object.size(get(x, envir = .GlobalEnv)), unit = 'auto'))[
                order(sapply(ls(envir = .GlobalEnv),function(x){object.size(get(x, envir = .GlobalEnv))}))
              ]
              
          }
