package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeAssignment(val targets: List[Either[NodeLTuple, NodeValue]], val expr: Either[NodeRTuple, NodeExpr]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
