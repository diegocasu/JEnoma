mkdir ebin
erl -make
mvn clean install
java -cp target/jenoma-1.0-jar-with-dependencies.jar it.unipi.jenoma.example.knapsack_problem.KnapSackProblemAlgorithm configuration.json

