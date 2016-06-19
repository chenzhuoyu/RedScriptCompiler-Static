package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeFor(val target: Either[NodeLTuple, NodeValue], val expr: NodeExpr, val body: NodeStatement) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
