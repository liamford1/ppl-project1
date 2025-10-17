file://<WORKSPACE>/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala
### java.lang.IllegalArgumentException: Comparison method violates its general contract!

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 706
uri: file://<WORKSPACE>/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala
text:
```scala
package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.

        TODO: Implement this function.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(d) =>
                List(IPush(d))

            case Add(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e@@)

            case Subtract(e1, e2) =>

            case Let()
        }
    }

}

```



#### Error stacktrace:

```
java.base/java.util.TimSort.mergeLo(TimSort.java:781)
	java.base/java.util.TimSort.mergeAt(TimSort.java:518)
	java.base/java.util.TimSort.mergeCollapse(TimSort.java:448)
	java.base/java.util.TimSort.sort(TimSort.java:245)
	java.base/java.util.Arrays.sort(Arrays.java:1230)
	scala.collection.SeqOps.sorted(Seq.scala:728)
	scala.collection.SeqOps.sorted$(Seq.scala:719)
	scala.collection.immutable.List.scala$collection$immutable$StrictOptimizedSeqOps$$super$sorted(List.scala:79)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted(StrictOptimizedSeqOps.scala:82)
	scala.collection.immutable.StrictOptimizedSeqOps.sorted$(StrictOptimizedSeqOps.scala:82)
	scala.collection.immutable.List.sorted(List.scala:79)
	dotty.tools.pc.completions.Completions.completions(Completions.scala:145)
	dotty.tools.pc.completions.CompletionProvider.completions(CompletionProvider.scala:139)
	dotty.tools.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:150)
```
#### Short summary: 

java.lang.IllegalArgumentException: Comparison method violates its general contract!