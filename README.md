# JEnoma

Project for the Distributed Systems and Middleware Technologies course of the Master of Science in 
Computer Engineering, University of Pisa.

The project consists in the design and implementation of a Java library for the distributed execution of
genetic algorithms on a set of remote machines, exploiting Erlang to manage the cluster and the communication
between nodes.

## Requirements
- A cluster of N+1 machines, composed of N workers and 1 coordinator, where a machine cannot be a worker
  and a coordinator at the same time; clusters composed of machines with different operating systems
  — Windows and Linux distributions — are allowed.
  The coordinator is implicitly defined as the machine that starts the execution of the program.
- Java JDK 16, Erlang 24, SSH and SCP installed in all the machines.
- Maven 3.8.1 installed in the coordinator.
- The ```PATH``` environment variable of the machines must be updated so that the commands
  ```java, jar, ssh, scp, mvn``` can be executed directly from the shell.
- If the coordinator is a Linux machine, the shell process must be invocable with the command ```bash```.
  
## Installation
- Assign a symbolic name to each machine of the cluster. In the following, it is assumed that the 
  workers are named ```worker1,...,workerN``` and the coordinator is named ```coordinator```.
- For each machine, add to ```/etc/hosts``` or to ```C:\Windows\System32\drivers\etc\hosts``` 
  the entries mapping the symbolic names to the corresponding IP addresses. 
- Create a user with the same name in each worker machine, for example named ```jenoma```. 
  The files transferred by the coordinator to a worker are saved in the home directory of this user,
  inside the ```.jenoma``` folder.
- Setup the coordinator so that it can access via SSH/SCP each worker, using SSH keys and authenticating 
  as the previously created user.
  For each worker, a couple of private/public keys must be created, with the public key that must 
  be stored inside the ```authorized_keys``` file of the worker itself. All the private keys 
  must be stored in the coordinator inside a single folder of choice, where each key file must be named
  as the corresponding machine's symbolic name, without file extensions. 
  For instance, the file holding the private key of ```worker1``` must be named exactly ```worker1```.
- Test the SSH setup before running a program or an example, being sure that the workers are correctly added 
  inside the ```known_hosts``` file of the coordinator.

## Execution
- Write the Java classes used to solve your problem, placing them in a dedicated package 
  of the project, or choose an available example in ```it.unipi.jenoma.example```.
  There is no need to move the files manually to the remote machines or start remote processes: 
  everything is distributed automatically by the coordinator at run time.
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
    "timeoutSetupCluster": 30000,
    "timeoutWorker": 100000,
    "seed": 1
  }
  ```
- Run your program or an available example with the command ```java -cp jarPath yourpackage.yourclass```.
  For instance, the TSP and knapsack examples can be launched with:
  ```bash
  java -cp target/jenoma-1.0-jar-with-dependencies.jar it.unipi.jenoma.example.TSP configuration.json
  ```
    ```bash
  java -cp target/jenoma-1.0-jar-with-dependencies.jar it.unipi.jenoma.example.KnapsackProblem configuration.json
  ```
