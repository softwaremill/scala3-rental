# Scala3 Scale By The Bay workshop

Welcome!

We'll be coding a simple HTTP API using Scala 3. Your task is to follow along and ask questions :)

Here's what you'll need to do beforehand:

1. clone this repository: `git clone https://github.com/softwaremill/scala3-rental-seed.git`
2. [install sbt](https://www.scala-sbt.org)
3. in the `scala3-rental-seed` directory, run the sbt console using `sbt`. This should bring up the prompt:

```
sbt:scala3-rental-seed>
```

4. type `compile`. This should download the necessary dependencies and compile the `HelloWorld` source code. You should
   see a green `[success]` message shortly.
5. type `run`. This will run the sole main method defined in `src/scala/rental/HelloWorld.scala`, and should output:

```
[info] running rental.helloWorld
Hello, world!
[success] Total time: 1 s, completed Oct 22, 2021, 12:16:56 PM
```

6. although you can edit the source code in any text editor, it's more convenient to use an IDE. You can use
   [IntelliJ IDEA with the Scala plugin](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html), 
   which has decent support for Scala3, although there are some rough edges. Or, if you are a VS Code user, try
   [Metals](https://scalameta.org/metals/). 
7. try importing the project and running our `Hello, world!` example from the IDE. Note to IntelliJ users: you'll
   most probably need to change the default way of importing, so that it is done through sbt. To configure, after 
   opening the project, go to the `sbt` tab on the right, click on the wrench on the far right ("Build Tool Settings"),
   choose "sbt settings", and tick the two checkboxes next to "sbt shell - use for project reload" and "build". Finally,
   reload (again on the sbt tab) and you're good to go.

## Exploring Scala further

* Our Scala starting page: resources, application stacks, guides: [scala.page](https://softwaremill.com/scala/)
* Our weekly Scala newsletter, [Scala Times](https://scalatimes.com)