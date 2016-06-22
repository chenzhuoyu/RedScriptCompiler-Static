package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeRaise(val expr: NodeExpr) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
