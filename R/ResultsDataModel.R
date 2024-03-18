# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of CohortDiagnostics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#' Get specifications for Cohort Diagnostics results data model
#'
#' @return
#' A tibble data frame object with specifications
#'
#' @export
getResultsDataModelSpecifications <- function() {
  readr::local_edition(1)
  pathToCsv <-
    system.file("settings", "resultsDataModelSpecification.csv", package = utils::packageName())
  resultsDataModelSpecifications <-
    readr::read_csv(file = pathToCsv, col_types = readr::cols())

  colnames(resultsDataModelSpecifications) <- SqlRender::snakeCaseToCamelCase(colnames(resultsDataModelSpecifications))
  return(resultsDataModelSpecifications)
}

#' Get a list of vocabulary table names
#'
#' @return
#' Get a list of vocabulary table names in results data model
#'
#' @export
getDefaultVocabularyTableNames <- function() {
  getResultsDataModelSpecifications() %>%
    dplyr::filter(.data$isVocabularyTable == "Yes") %>%
    dplyr::pull(.data$tableName) %>%
    unique() %>%
    sort() %>%
    SqlRender::snakeCaseToCamelCase()
}


# Private function for testing migrations in isolation
.createDataModel <- function(connection, databaseSchema, tablePrefix) {
  sqlParams <- getPrefixedTableNames(tablePrefix)
  sql <- do.call(
    SqlRender::loadRenderTranslateSql,
    c(
      sqlParams,
      list(
        sqlFilename = "CreateResultsDataModel.sql",
        packageName = utils::packageName(),
        dbms = connection@dbms,
        results_schema = databaseSchema
      )
    )
  )
  DatabaseConnector::executeSql(connection, sql)
}

#' Create the results data model tables on a database server.
#'
#' @details
#' Only PostgreSQL servers are supported.
#'
#' @param connectionDetails      DatabaseConnector connectionDetails instance @seealso[DatabaseConnector::createConnectionDetails]
#' @param databaseSchema         The schema on the postgres server where the tables will be created.
#' @param tablePrefix            (Optional)  string to insert before table names (e.g. "cd_") for database table names
#' @export
createResultsDataModel <- function(connectionDetails = NULL,
                                   databaseSchema,
                                   tablePrefix = "") {
  if (connectionDetails$dbms == "sqlite" & databaseSchema != "main") {
    stop("Invalid schema for sqlite, use databaseSchema = 'main'")
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  .createDataModel(connection, databaseSchema, tablePrefix)
  migrateDataModel(
    connectionDetails = connectionDetails,
    databaseSchema = databaseSchema,
    tablePrefix = tablePrefix
  )
}

#' Upload results to the database server.
#'
#' @description
#' Requires the results data model tables have been created using the \code{\link{createResultsDataModel}} function.
#'
#' Set the POSTGRES_PATH environmental variable to the path to the folder containing the psql executable to enable
#' bulk upload (recommended).
#'
#' @param connectionDetails   An object of type \code{connectionDetails} as created using the
#'                            \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                            DatabaseConnector package.
#' @param schema         The schema on the postgres server where the tables have been created.
#' @param zipFileName    The name of the zip file.
#' @param forceOverWriteOfSpecifications  If TRUE, specifications of the phenotypes, cohort definitions, and analysis
#'                       will be overwritten if they already exist on the database. Only use this if these specifications
#'                       have changed since the last upload.
#' @param purgeSiteDataBeforeUploading If TRUE, before inserting data for a specific databaseId all the data for
#'                       that site will be dropped. This assumes the input zip file contains the full data for that
#'                       data site.
#' @param tempFolder     A folder on the local file system where the zip files are extracted to. Will be cleaned
#'                       up when the function is finished. Can be used to specify a temp folder on a drive that
#'                       has sufficient space if the default system temp space is too limited.
#' @param tablePrefix    (Optional)  string to insert before table names (e.g. "cd_") for database table names
#' @param ...            See ResultModelManager::uploadResults
#' @export
uploadResults <- function(connectionDetails,
                          schema,
                          zipFileName = NULL,
                          dataFolder = NULL,
                          forceOverWriteOfSpecifications = FALSE,
                          purgeSiteDataBeforeUploading = TRUE,
                          tempFolder = tempdir(),
                          tablePrefix = "",
                          ...) {

  if (is.null(zipFileName) & is.null(dataFolder))
    stop("Must specify output folder or zip")

  if (!is.null(zipFileName)) {
    unzipFolder <- tempfile("unzipTempFolder", tmpdir = tempFolder)
    dir.create(path = unzipFolder, recursive = TRUE)
    on.exit(unlink(unzipFolder, recursive = TRUE), add = TRUE)

    ParallelLogger::logInfo("Unzipping ", zipFileName)
    zip::unzip(zipFileName, exdir = unzipFolder)
    dataFolder <- unzipFolder
  } else {
    if (!dir.exists(dataFolder))
      stop("Cannot find data folder")
  }

  ResultModelManager::uploadResults(
    connectionDetails = connectionDetails,
    schema = schema,
    resultsFolder = dataFolder,
    tablePrefix = tablePrefix,
    forceOverWriteOfSpecifications = forceOverWriteOfSpecifications,
    purgeSiteDataBeforeUploading = purgeSiteDataBeforeUploading,
    runCheckAndFixCommands = TRUE,
    databaseIdentifierFile = "database.csv",
    specifications = getResultsDataModelSpecifications(),
    warnOnMissingTable = FALSE,
    ...
  )
}

#' Migrate Data model
#' @description
#' Migrate data from current state to next state
#'
#' It is strongly advised that you have a backup of all data (either sqlite files, a backup database (in the case you
#' are using a postgres backend) or have kept the csv/zip files from your data generation.
#'
#' @inheritParams getDataMigrator
#' @export
migrateDataModel <- function(connectionDetails, databaseSchema, tablePrefix = "") {
  ParallelLogger::logInfo("Migrating data set")
  migrator <- getDataMigrator(connectionDetails = connectionDetails, databaseSchema = databaseSchema, tablePrefix = tablePrefix)
  migrator$executeMigrations()
  migrator$finalize()

  ParallelLogger::logInfo("Updating version number")
  updateVersionSql <- SqlRender::loadRenderTranslateSql("UpdateVersionNumber.sql",
                                                        packageName = utils::packageName(),
                                                        database_schema = databaseSchema,
                                                        table_prefix = tablePrefix,
                                                        dbms = connectionDetails$dbms
  )

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  DatabaseConnector::executeSql(connection, updateVersionSql)
}


#' Get database migrations instance
#' @description
#'
#' Returns ResultModelManager DataMigrationsManager instance.
# '@seealso [ResultModelManager::DataMigrationManager] which this function is a utility for.
#'
#' @param connectionDetails             DatabaseConnector connection details object
#' @param databaseSchema                String schema where database schema lives
#' @param  tablePrefix                  (Optional) Use if a table prefix is used before table names (e.g. "cd_")
#' @returns Instance of ResultModelManager::DataMigrationManager that has interface for converting existing data models
#' @export
getDataMigrator <- function(connectionDetails, databaseSchema, tablePrefix = "") {
  ResultModelManager::DataMigrationManager$new(
    connectionDetails = connectionDetails,
    databaseSchema = databaseSchema,
    tablePrefix = tablePrefix,
    migrationPath = "migrations",
    packageName = utils::packageName()
  )
}

# Extract all csv data from a database and dump to csv files
extractCsvFromDb <- function(outputFolder,
                             overwrite = FALSE,
                             tablePrefix = "",
                             sqliteDbPath = "MergedCohortDiagnosticsData.sqlite",
                             databaseSchema = "main",
                             connectionDetails = NULL) {

  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE)

  if (is.null(connectionDetails)) {
    checkmate::assertFileExists(sqliteDbPath)
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqliteDbPath)
  }

  if (dir.exists(outputFolder) & !overwrite) {
    stop("Output directory exists. Use overwrite to continue")
  }

  dir.create(outputFolder, showWarnings = FALSE)

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  dataModel <- getResultsDataModelSpecifications()


  writeData <- function(x, pos, tableName) {
    # Hack to convert date types from sqlite
    if (DatabaseConnector::dbms(connection) == "sqlite") {
      colSpecs <- dataModel |>
        dplyr::filter(.data$tableName == !!tableName,
                      tolower(.data$dataType) == "date")

      for (colname in colSpecs$columnName) {
        x[[colname]] <- lubridate::as_date(lubridate::as_datetime(x[[colname]]))
      }
    }

    fileName <- file.path(outputFolder, paste0(tableName, ".csv"))
    readr::write_csv(x = x, file = fileName, append = pos != 1, na = '')
    return(NULL)
  }

  for (table in unique(dataModel$tableName)) {
    DatabaseConnector::renderTranslateQueryApplyBatched(connection = connection,
                                                        sql = "SELECT * FROM @schema.@table_prefix@table",
                                                        fun = writeData,
                                                        table_prefix = tablePrefix,
                                                        table = table,
                                                        schema = databaseSchema,
                                                        args = list(tableName = table))
  }
}