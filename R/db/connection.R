# R/db/connection.R

#' Create a database connection pool
#'
#' @param db_name Database name
#' @param db_user Username
#' @param db_password Password
#' @param db_host Host
#' @param db_port Port
#' @return A pool object
#' @export
create_db_pool <- function(
    db_name,
    db_user,
    db_password,
    db_host = "localhost",
    db_port = 3306
) {
  pool <- pool::dbPool(
    drv = RMySQL::MySQL(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password,
    # Important for reconnection behavior
    onConnect = function(conn) {
      DBI::dbExecute(conn, "SET SESSION wait_timeout=5400;")  # 90 minutes timeout
      DBI::dbExecute(conn, "SET NAMES utf8mb4;") 
    },
    # Add these pool management parameters
    idleTimeout = 60 * 60 * 1000,     # 1 hour in milliseconds
    validationInterval = 30 * 1000    # 30 seconds in milliseconds
  )
  
  return(pool)
}

#' Check if a database connection is valid and working
#'
#' @param pool Database connection pool
#' @return Logical indicating if connection is working
#' @export
check_db_connection <- function(pool) {
  # First check if the pool object exists and is not NULL
  if (is.null(pool)) {
    return(FALSE)
  }
  
  # Check if the pool is a valid pool object
  if (!inherits(pool, "Pool")) {
    message("Invalid pool object type: ", class(pool)[1])
    return(FALSE)
  }
  
  # Use a safer approach to check pool validity
  is_valid <- tryCatch({
    pool$valid
  }, error = function(e) {
    message("Pool validity check failed: ", e$message)
    return(FALSE)
  })
  
  if (!is_valid) {
    return(FALSE)
  }
  
  # If the pool is valid, try a simple query to test the connection
  tryCatch({
    test_query <- pool::poolWithTransaction(pool, function(conn) {
      DBI::dbGetQuery(conn, "SELECT 1 AS test")
    })
    
    if (nrow(test_query) > 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    message("Connection test query failed: ", e$message)
    return(FALSE)
  })
}

#' Create a global database reconnection function
#'
#' This function creates a closure that contains your database credentials
#' and can be used to recreate the pool if needed.
#'
#' @param db_name Database name
#' @param db_user Username
#' @param db_password Password
#' @param db_host Host
#' @param db_port Port
#' @return A function that when called creates a new pool
#' @export
create_db_reconnector <- function(
    db_name,
    db_user,
    db_password,
    db_host = "localhost",
    db_port = 3306
) {
  function() {
    # Create a new pool
    create_db_pool(
      db_name = db_name,
      db_user = db_user,
      db_password = db_password,
      db_host = db_host,
      db_port = db_port
    )
  }
}

#' Safely close a database connection pool
#'
#' @param pool Database connection pool
#' @return NULL invisibly
#' @export
safe_pool_close <- function(pool) {
  if (is.null(pool)) {
    return(invisible(NULL))
  }
  
  tryCatch({
    # Try to check if the pool is valid first
    is_valid <- FALSE
    
    is_valid <- tryCatch({
      pool$valid
    }, error = function(e) {
      message("Pool validity check failed during close: ", e$message)
      return(FALSE)
    })
    
    # Only try to close if it's valid
    if (is_valid) {
      pool$close()
      message("Database pool successfully closed")
    } else {
      message("Pool was already closed or invalid")
    }
  }, error = function(e) {
    message("Error closing pool: ", e$message)
  })
  
  return(invisible(NULL))
}

# Initialize global connection pool
message("Initializing global database connection pool...")

# Get database credentials from environment variables or use defaults
db_creds <- list(
  db_name = Sys.getenv("DB_NAME", "avp36"),
  db_user = Sys.getenv("DB_USER", ""),  # Use your default username
  db_password = Sys.getenv("DB_PASS", ""),  # Use your default password
  db_host = Sys.getenv("DB_HOST", "localhost"),
  db_port = as.numeric(Sys.getenv("DB_PORT", "3306"))
)

tryCatch({
  # Create the global pool object
  pool <<- create_db_pool(
    db_name = db_creds$db_name,
    db_user = db_creds$db_user,
    db_password = db_creds$db_password,
    db_host = db_creds$db_host,
    db_port = db_creds$db_port
  )
  
  # Create the global reconnector function
  reconnect_db <<- create_db_reconnector(
    db_name = db_creds$db_name,
    db_user = db_creds$db_user,
    db_password = db_creds$db_password,
    db_host = db_creds$db_host,
    db_port = db_creds$db_port
  )
  
  # Test if pool is working
  if (check_db_connection(pool)) {
    message("Database connection pool successfully initialized")
  } else {
    warning("Pool created but connection test failed - check credentials and connection parameters")
  }
}, error = function(e) {
  warning("Failed to create database connection pool: ", e$message)
  # Create NULL placeholders in global environment
  pool <<- NULL
  reconnect_db <<- function() {
    warning("Connection reconnection failed - original connection was never established")
    return(NULL)
  }
})