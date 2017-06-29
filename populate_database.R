
composeUserAddQuery <- function(user, password, email, role) 
  # convenience function to salt and hash a password before composing
  # a query to add it to the database
  {
  sodium::password_store(password) %>%
    sprintf("INSERT INTO pw VALUES('%s', '%s', '%s', '%s')", user, ., email, role)
}

sendUserAddQuery <- function(user, password, email, role) 
  # convenience function to call the composer and send the query
  # returning named booleans indicating the success of each row
  {
  composeUserAddQuery(user, password, email, role) %>%
    dbSendQuery(db, .) %>%
    dbClearResult()
}

# initialize a connection, will create a db if not exists
db <- dbConnect(SQLite(), dbname = database)

# create the table for the logins
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS pw'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE pw (user TEXT, password TEXT, email TEXT, role TEXT)'))

# initialize a DT of some dummy logins
db_logins <- data.table::data.table(
  user = c('Mac', 'Dan', 'Norbert'),
  role = c('Consultant', 'Principal-Manager', 'Principal'),
  password = rep("Welcome1", 3)
)
db_logins[, email := paste0(user, "@me.com")]

# perform additions
success <- db_logins[, mapply(sendUserAddQuery, user, password, email, role)]

# check that all are TRUE
stopifnot(all(success))

dbDisconnect(db)
