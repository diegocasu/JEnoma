# JEnoma

## Requirements
- A cluster of N+1 machines, composed of N workers and 1 coordinator.
- Java JDK 16, Erlang 24, SSH and SCP installed on all the machines.
- Maven installed only on the coordinator.
- The ```PATH``` variable of all the machines must be set so that the commands
  ```java, jar, ssh, scp, mvn``` can be executed directly from the shell.
- If the coordinator is a Linux machine, the shell process must be invocable with the command ```sh```.
  
## Installation
- Give a symbolic name to each machine of the cluster. In the following, it is assumed that the workers 
  are called ```worker1, ... , workerN``` and the coordinator ```coordinator```.
- For each machine in the cluster, add to the ```etc/hosts``` file N+1 entries, each one mapping a
  symbolic name to the corresponding IP address of the node.
- Create a user with the same name (ex. ```jenoma```) in each worker.
- Setup the coordinator so that it can access via SSH/SCP each worker, using SSH keys and authenticating
  itself as the user ```jenoma```. The private keys held by the coordinator must be stored in a single folder
  of choice, where each key file must be named as the corresponding machine's symbolic name (ex. the file with 
  the private key used to access ```worker1``` must be named exactly _worker1_).

## Execution
- Run the build script, either ```build.sh``` or ```build.bat```.
- Prepare a JSON configuration file ```configuration.json```, placed in the same directory of the project,
  with the following structure:
  ```
   {
    "jarPath": "./target/jenoma-1.0-jar-with-dependencies.jar",
    "sshKeyFolder": "your_path/your_SSH_key_folder",
    "sshUser": "jenoma",
    "coordinator": "coordinator",
    "workers": [
      "worker1",
      ...
      "workerN"
    ],
    "timeoutSetupCluster": 10000,
    "timeoutWorker": 100000,
    "seed": 1
  }
    ```
  The timeout is in milliseconds.
- Run the program:
  ```bash
    sudo java -cp target/jenoma-1.0-jar-with-dependencies.jar it.unipi.jenoma.cluster.Coordinator
  ```