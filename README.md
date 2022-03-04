### Distributed Associative Bandit Tree Search (DABTree)

This library explores the possibility of using Monte Carlo Tree Search for arbitrary combinatorial search problems. Some search problems have interdependent costs, which is difficult to model using standard ILP approaches. The probabilistic nature of the MCTS tree makes it suitable for these sorts of problems.

DABTree under research to solve route guidance problems with a social equilibrium objective. It extends MCTS

### Installation

DABTree depends on the following Scala libraries:
- [Cats](https://github.com/typelevel/cats) 
- [Spire](https://github.com/non/spire)

See __releases__ for .jar files.

### Usage

Consider the example in `com.github.robfitzgerald.dabtree.local.example.CombSearchTests`. The problem is to find a vector of values, each in the range [0,1]. 

To do this in a local context (storing tree operations in a List and performing samples synchronously), a `LocalSyncSearch` can be called. 

The user must define/provide the following functions and parameters:

- simulate: adds (arbitrarily) to a partial solution state to make it a complete solution state
- evaluate: determines the cost of a solution state
- objective: Minimize, or Maximize
- generateChildren: generates possible child states by adding Actions to a partial solution state
- rankingPolicy: ranks a payload based on its state, reward, stats, or other details. a few example generic Ranking functions are provided.
- allowChildPromotion: a function that is true if promotion is allowed for the provided child.
- activatedPayloadLimit: number of payloads that can be sampling each iteration, based on rank
- totalPayloadCapacity: the number beyond which payloads will be permanently cancelled based on rank
- startFrontier: the position to start the search from; for example, if the state type is a `List[Double]` and Action a `Double`, starting from an empty tree is providing `List((List(), Option.empty[Double]))`.


### References

[Mougouei, Davoud, David MW Powers, and Asghar Moeini. "An integer linear programming model for binary knapsack problem with dependent item values." Australasian Joint Conference on Artificial Intelligence. Springer, Cham, 2017.](https://link.springer.com/chapter/10.1007/978-3-319-63004-5_12)

[Browne, Cameron B., et al. "A survey of monte carlo tree search methods." IEEE Transactions on Computational Intelligence and AI in games 4.1 (2012): 1-43.
](https://ieeexplore.ieee.org/abstract/document/6145622)

[Pedroso, João Pedro, and Rui Rei. "Tree Search and Simulation." Applied Simulation and Optimization. Springer, Cham, 2015. 109-131.](https://link.springer.com/chapter/10.1007/978-3-319-15033-8_4)
