error id: file://<WORKSPACE>/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala:scala/package.List.
file://<WORKSPACE>/src/main/scala/edu/colorado/csci3155/project1/StackMachineCompiler.scala
empty definition using pc, found symbol in pc: scala/package.List.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -List.
	 -List#
	 -List().
	 -scala/Predef.List.
	 -scala/Predef.List#
	 -scala/Predef.List().
offset: 1711
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
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IPlus)

            case Sub(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(ISub)
            
            case Mul(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IMul)

            case Div(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IDiv)
            
            case Geq(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IGeq)
            
            case Gt(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IGt)
            
            case Eq(e1, e2) =>
                compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(IEq)

            case Not(e) =>
                compileToStackMachineCode(e) ++ List(INot)
            
            case Let(x, e1, e2) =>
                compileToStackMachineCode(e1) ++ Lis@@t(IStore(x)) ++ compileToStackMachineCode(e2) ++ List(IPop)
            
            case Id(str) =>
                List(ILoad(str))
        }
    }

}

```


#### Short summary: 

empty definition using pc, found symbol in pc: scala/package.List.