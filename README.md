# vtg

R package that supports running algorithms on the [Vantage6](https://github.com/IKNL/VANTAGE6) federated infrastructure. Contains a `Client` and functions that facillitate running dockerized algorithms.

## Installation
Run the following in the R console to install the package and its dependencies:
```R
devtools::install_github('mellesies/vtg')
```

## Example use
```R
# Function to create a client
setup.client <- function() {
  # Define parameters
  username <- "username@example.com"
  password <- "password"
  host <- 'https://api-test.distributedlearning.ai'
  api_path <- ''
  
  # Create the client
  client <- vtg::Client$new(host, api_path=api_path)
  client$authenticate(username, password)

  return(client)
}

# Create a client
client <- setup.client()

# Get a list of all collaborations our user is 
client$getCollaborations()

# Select a collaboration
client$setCollaborationId(1)

# Call a dockerized algorithm to get a list of all column names
image.name <- "harbor.distributedlearning.ai/vantage/vtg.basic:test"

client$set.task.image(
    image.name,
    task.name="colnames"
)

result <- client$call("colnames")
print(result)
```

