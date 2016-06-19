package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeExpr(val left: Either[NodeExpr, NodeValue], val op: String, val right: NodeExpr) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
