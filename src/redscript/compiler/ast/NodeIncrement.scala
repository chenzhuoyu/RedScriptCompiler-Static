package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeIncrement(val target: NodeValue, val op: String, val expr: NodeExpr) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
