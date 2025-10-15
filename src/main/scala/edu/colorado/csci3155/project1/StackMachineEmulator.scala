package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- TODO: Complete the inductive definition of the remaining 
          byte code instructions as specified 
          in the documentation --*/
case class IPush(d: Double) extends StackMachineInstruction
case class IPushBool(b: Boolean) extends StackMachineInstruction
case object IPop extends StackMachineInstruction
case object IPlus extends StackMachineInstruction
case object ISub extends StackMachineInstruction
case object IMul extends StackMachineInstruction
case object IDiv extends StackMachineInstruction
case object IGeq extends StackMachineInstruction
case object IGt extends StackMachineInstruction
case object IEq extends StackMachineInstruction
case object INot extends StackMachineInstruction
case class IStore(id: String) extends StackMachineInstruction
case class ILoad(id: String) extends StackMachineInstruction

case class ICondSkip(n: Int) extends StackMachineInstruction
case class ISkip(n: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type EnvStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
        TODO: Implement this function.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: EnvStack,
                                 ins: StackMachineInstruction): (OpStack, EnvStack) = {
        ins match {
            case IPush(d) =>
                (Num(d) :: stack, env)

            case IPushBool(b) => 
                (Bool(b) :: stack, env)

            case IPop =>
                stack match {
                    case _ :: rest => (rest, env)
                    case Nil => throw new IllegalArgumentException("Cant pop from empty stack")
                }

            case IPlus =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Num(v1+v2) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ =>
                        throw new IllegalArgumentException("Stack needs 2 values")
                }
            
            case ISub =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Num(v2-v1) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ =>
                        throw new IllegalArgumentException("Stack needs 2 values")
                }
            
            case IMul =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Num(v1*v2) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ => 
                        throw new IllegalArgumentException("Stack needs 2 values")
                }
            
            case IDiv =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        if (v1 == 0.0) throw new IllegalArgumentException("Divison by 0") 
                        (Num(v2/v1) :: rest, env) 
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ => 
                        throw new IllegalArgumentException("Stack needs 2 values")
                }

            case IGeq =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Bool(v2>=v1) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ => 
                        throw new IllegalArgumentException("Stack needs 2 values")
                }

            case IGt =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Bool(v2>v1) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ => 
                        throw new IllegalArgumentException("Stack needs 2 values")
                }
            
            case IEq =>
                stack match {
                    case Num(v1) :: Num(v2) :: rest =>
                        (Bool(v2==v1) :: rest, env)
                    case _ :: _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be numbers")
                    case _ => 
                        throw new IllegalArgumentException("Stack needs 2 values")
                }
            
            case INot =>
                stack match {
                    case Bool(b) :: rest =>
                        (Bool(!b) :: rest, env)
                    case _ :: _ =>
                        throw new IllegalArgumentException("Type error, must be boolean")
                    case Nil => 
                        throw new IllegalArgumentException("Stack needs a value")
                }
                
            case IStore(id) =>
                stack match {
                    case v :: rest =>
                        (rest, (id, v) :: env)
                    case Nil => 
                        throw new IllegalArgumentException("Stack needs a value")
                }
            
            case ILoad(id) =>
                env.find { case (name, _) => name == id } match {
                case Some((_, value)) =>
                    (value :: stack, env)
                case None =>
                    throw new IllegalArgumentException("Variable not found")
            }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: EnvStack=Nil): (Value, EnvStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case ICondSkip(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case ISkip(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:EnvStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}