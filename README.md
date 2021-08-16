<h1 align="center">
  <br>
  <a href="https://vantage6.ai"><img src="https://github.com/IKNL/guidelines/blob/master/resources/logos/vantage6.png?raw=true" alt="vantage6" width="400"></a>
</h1>

<h3 align=center> A privacy preserving federated learning solution</h3>

--------------------

# vtg
R package that supports running algorithms on the [Vantage6](https://github.com/IKNL/VANTAGE6) federated infrastructure. 
Contains a `Client` and functions that facilitate running dockerized algorithms.

The user can also specify the data format for the input and output of an algorithm. 
The client and wrapper handle the (de)serialization for this. 
This feature makes it possible to run R-based algorithms from the Python client as well.

Note: If you want cross-language algorithms be sure that, while creating an algorithm, the type of your results can be converted to json.

## Installation
Run the following in the R console to install the package and its dependencies:
```R
devtools::install_github('IKNL/vtg')
```

## Usage
```R
# Function to create a client
setup.client <- function() {
    # Define parameters
    username <- "username@example.com"
    password <- "password"
    host <- "https://harukas.vantage6.ai:443"
    api_path <- ""
    
    # Create the client
    client <- vtg::Client$new(host, api_path)
    client$authenticate(username, password)

    return(client)
}

# Create a client
client <- setup.client()

# Get a list of available collaborations
print(client$getCollaborations())

# Could output something like this:
#   id     name
# 1  1 ZEPPELIN
# 2  2 PIPELINE

# Select a collaboration
client$setCollaborationId(1)

# Data format can be set to either 'json' or 'rds' (default)
client$data_format <- 'json'

# User can specify to use the master function instead of RPC calls
client$use.master.container <- TRUE
```

Then the client can be used to create new (sub-)tasks and retrieve the results. 

### Run an algorithm
For example if we want to run the `master` method (dcoxph) from the Cox Proportional Hazards (coxph) algorithm. 

Run the following in the R console to install the package and its dependencies:
```R
# This also installs the package vtg
devtools::install_github('mellesies/vtg.coxph', subdir="src")
```

Run the following for the coxph model:
```R
# Define explanatory variables, time column and censor column
expl_vars <- c("Age","Race2","Race3","Mar2","Mar3","Mar4","Mar5","Mar9",
               "Hist8520","hist8522","hist8480","hist8501","hist8201",
               "hist8211","grade","ts","nne","npn","er2","er4")
time_col <- "Time"
censor_col <- "Censor"

# The master function of vtg.coxph is called 'dcoxph'
result <- vtg.coxph::dcoxph(client, expl_vars, time_col, censor_col)

print(result)
```
------------------------------------
> [vantage6](https://vantage6.ai)
