# configuration from config.yml

this_configuration <- config::get(config = "default")

# db connection by function
# pool not used owing to leaking

database_connection <- function() {

  dbConnect(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = this_configuration$dbname,
    host = this_configuration$host,
    user = this_configuration$user,
    password = this_configuration$password
  )

}
