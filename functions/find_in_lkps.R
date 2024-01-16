#to fetch values from look up

find_in_lkp <- function(db_lkp,
                        fetch,
                        when,
                        equals
){
  
  db_lkp[fetch][db_lkp[when] == equals]
  
}
